{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import Control.Logging qualified as Log
import Control.Monad.Except (ExceptT, forM, lift, runExceptT, throwError, when, zipWithM)
import FileHandling
import PBHModel
import System.Environment
import System.Exit
import System.TimeIt qualified as Time
import System.Timeout
import Algorithm 
import PVGrammar.Parse (protoVoiceEvaluator)
import qualified Algorithm as Core
import Control.Monad (replicateM)
import HeuristicParser (chordAccuracy)

data Options = Options
  { _inputPath :: String
  , _outputPath :: String
  , _iterations :: Int
  }

-- COMAND LINE ARGUMENT HANDLING
parseArgs :: Options -> [String] -> IO (String, String, AlgoType, Options)
parseArgs _ ["-h"] = usage >> exit
parseArgs _ ["-v"] = version >> exit
parseArgs options ("-i" : inputPath : rst) = parseArgs (options{_inputPath = inputPath}) rst
parseArgs options ("-o" : outputPath : rst) = parseArgs (options{_outputPath = outputPath}) rst
parseArgs options ("-n" : numIterations : rst) = parseArgs (options{_iterations = read numIterations}) rst
parseArgs options [corpus, pieceName, algoName] = pure (corpus, pieceName, read algoName, options) -- concat `fmap` mapM readFile fs
parseArgs _ _ = usage >> exit

defaultInputPath = "preprocessing/inputs/"
defaultOutputPath = "preprocessing/outputs/"
defaultNumIterations = 1

usage =
  putStrLn
    "\nUsage: parseFullPieces [-vhio] corpus piece {RandomParse, RandomParseSBS, RandomSample, Heuristic1, HeuristicSBS1, All} \n\
    \   -v:         Show Version \n\
    \   -h:         Show Help \n\
    \   -i:         Set input path for chords and slices. Default: preprocessing/inputs/ \n\
    \   -o:         Set output path for results. Default: preprocessing/outputs/ \n\
    \   -n:         Set number of iterations \n\
    \   corpus:     Name of the corpus the piece belongs to. \n\
    \   pieceName:  Name of piece to parse. \n\
    \   {..}:       Choose which algorithm to run. \"all\" runs all algorithms and returns results in an aggregated\
    \ json file\n\
    \               Options:\n\
    \                  = RandomParse\n\
    \                  | RandomParseSBS\n\
    \                  | RandomSample \n\
    \                  | RandomSampleSBS\n\
    \                  | Heuristic1 \n\
    \                  | HeuristicSBS1 \n\
    \                  | All "
version = putStrLn "Version 0.1"
exit = exitSuccess
die = exitWith (ExitFailure 1)

timeOutMs = 400 * 1000000 :: Int
numRetries = 1 :: Int

main :: IO () 
main = Log.withStderrLogging $ do 
  params <- loadParams "preprocessing/dcml_params.json"
  (corpus, pieceName, algo, Options inputPath outputPath iterations) <-
    getArgs
      >>= parseArgs (Options defaultInputPath defaultOutputPath defaultNumIterations)

  inputChords <- chordsFromFile (inputPath <> "chords/" <> corpus <> "/" <> pieceName <> ".csv")
  inputSlices <- slicesFromFile' (inputPath <> "slices/" <> corpus <> "/" <> pieceName <> ".csv")
  let outputFile = outputPath <> corpus <> "/" <> pieceName <> "/" <> show algo <> ".json"

  res <- replicateM iterations $ runAlgo algo params inputChords inputSlices numRetries

  writeJSONToFile outputFile $ concatResults corpus pieceName inputChords res

  where 
    runAlgo algo _ _ _ 0 = pure $ nullResultToJSON algo
    runAlgo algo params inputChords inputSlices n = do 
      mTimedRes <- timeout timeOutMs $ Time.timeItT $ runParse algo (AlgoInput protoVoiceEvaluator params inputSlices inputChords)
      case mTimedRes of 
        Nothing -> runAlgo algo params inputChords inputSlices (n - 1)
        Just (time, mRes) -> 
          case mRes of 
            Nothing -> pure $ nullResultToJSON algo
            Just (AlgoResult top ops lbls) -> 
              let accuracy = chordAccuracy inputChords lbls
                  likelihood = scoreSegments params scoreSegment' top lbls
                in 
                  pure $ writeResultsToJSON top lbls ops accuracy likelihood (show algo) time 
  
