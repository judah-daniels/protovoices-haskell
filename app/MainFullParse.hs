{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import Control.Logging qualified as Log
import Control.Monad.Except (ExceptT, forM, lift, runExceptT, throwError, when, zipWithM)
import FileHandling
import System.Environment
import System.Exit
import System.TimeIt qualified as Time
import System.Timeout
import Algorithm 
import PVGrammar.Parse (protoVoiceEvaluator)
import qualified Algorithm as Core
import Control.Monad (replicateM)
import HeuristicParser (chordAccuracy)
import Harmony
import Harmony.ChordLabel
import Harmony.Params

data Options = Options
  { _inputPath :: String
  , _outputPath :: String
  , _iterations :: Int
  , _beamWidth :: BeamWidth
  , _unsplitWidth :: UnsplitWidth
  , _unspreadWidth :: UnspreadWidth
  }

-- COMAND LINE ARGUMENT HANDLING
parseArgs :: Options -> [String] -> IO (String, String, AlgoType, Options)
parseArgs _ ["-h"] = usage >> exit
parseArgs _ ["-v"] = version >> exit
parseArgs options ("-i" : inputPath : rst) = parseArgs (options{_inputPath = inputPath}) rst
parseArgs options ("-o" : outputPath : rst) = parseArgs (options{_outputPath = outputPath}) rst
parseArgs options ("-n" : numIterations : rst) = parseArgs (options{_iterations = read numIterations}) rst
parseArgs options ("-p" : "beamWidth" : val : rst) = parseArgs (options{_beamWidth = read val}) rst
parseArgs options ("-p" : "unsplitWidth" : val : rst) = parseArgs (options{_unsplitWidth = read val}) rst
parseArgs options ("-p" : "unspreadWidth" : val : rst) = parseArgs (options{_unspreadWidth = read val}) rst
parseArgs options [corpus, pieceName, algoName] = pure (corpus, pieceName, read algoName, options) -- concat `fmap` mapM readFile fs
parseArgs _ _ = usage >> exit

defaultInputPath = "preprocessing/inputs/"
defaultOutputPath = "preprocessing/outputs/"
defaultNumIterations = 1
defaultUnspreadWidth = 7 
defaultUnsplitWidth = 3
defaultBeamWidth = 10

usage =
  putStrLn
    "\nUsage: parseFullPieces [-vhio] corpus piece {RandomParse, RandomParseSBS, RandomSample, Heuristic1, HeuristicSBS1, All} \n\
    \   -v:         Show Version \n\
    \   -h:         Show Help \n\
    \   -i:         Set input path for chords and slices. Default: preprocessing/inputs/ \n\
    \   -o:         Set output path for results. Default: preprocessing/outputs/ \n\
    \   -n:         Set number of iterations \n\
    \   -p {hp} value:         Hyperparameters \n\
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


main :: IO () 
main = Log.withStderrLogging $ do 
  (corpus, pieceName, algo, Options inputPath outputPath iterations beamWidth unsplitWidth unSpreadWidth) <-
    getArgs
      >>= parseArgs (Options defaultInputPath defaultOutputPath defaultNumIterations defaultBeamWidth defaultUnsplitWidth defaultUnspreadWidth)

  inputChords <- chordsFromFile (inputPath <> "chords/" <> corpus <> "/" <> pieceName <> ".csv")
  inputSlices <- slicesFromFile' (inputPath <> "slices/" <> corpus <> "/" <> pieceName <> ".csv")
  let outputFile = outputPath <> corpus <> "/" <> pieceName <> "/" <> showRoot algo <> ".json"

  res <- replicateM iterations $ runAlgo algo inputChords inputSlices numRetries

  writeJSONToFile outputFile $ concatResults corpus pieceName inputChords res

  where 
    timeOutMs = 400 * 1000000 :: Int

    numRetries = 1 :: Int

    runAlgo algo _ _ 0 = pure $ nullResultToJSON algo
    runAlgo algo inputChords inputSlices n = do 
      mTimedRes <- timeout timeOutMs $ Time.timeItT $ runParse algo (AlgoInput protoVoiceEvaluator inputSlices inputChords)
      case mTimedRes of 
        Nothing -> runAlgo algo inputChords inputSlices (n - 1)
        Just (time, mRes) -> 
          case mRes of 
            Nothing -> pure $ nullResultToJSON algo
            Just (AlgoResult top ops lbls) -> 
              let accuracy = chordAccuracy inputChords lbls
                  likelihood = scoreSegments top lbls
                in 
                  pure $ writeResultsToJSON top lbls ops accuracy likelihood (show algo) time 

    showRoot algo = 
      case algo of 
        BeamSearch width -> "BeamSearch_" <> show width
        DualBeamSearch a b -> "DualBeamSearch_" <> show a <> "_" <> show b
        _ -> show algo
  
