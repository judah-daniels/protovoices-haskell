{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import Control.Logging qualified as Log
import Data.Text qualified as T
import Control.Monad.Except (ExceptT, forM, lift, runExceptT, throwError, when, zipWithM)
import FileHandling
import System.Environment
import System.Directory
import System.Exit
import System.TimeIt qualified as Time
import System.Timeout
import Algorithm
import PVGrammar.Parse (protoVoiceEvaluator, protoVoiceEvaluatorImpure, protoVoiceEvaluatorLimitedSize)
import qualified Algorithm as Core
import Control.Monad (replicateM)
import HeuristicParser (chordAccuracy)
import Harmony
import Harmony.ChordLabel
import Harmony.Params
import Control.Logging (LogLevel(LevelDebug))
import Display
import PVGrammar.Generate
import GHC.IO.Handle (NewlineMode(outputNL))

data Options = Options
  { _inputPath :: String
  , _outputPath :: String
  , _iterations :: Int
  , _timeOut :: Int
  , _unsplitBias :: UnsplitBias
  , _childBias :: ChildBias
  , _expId :: String
  }

logD x = Log.log $ T.pack x
-- COMAND LINE ARGUMENT HANDLING
parseArgs :: Options -> [String] -> IO (String, String, AlgoType, Options)
parseArgs _ ["-h"] = usage >> exit
-- parseArgs _ ["-v"] = version >> exit
parseArgs _ ["-v"] = version >> exit
parseArgs options ("-v" : rst) = Log.setLogLevel LevelDebug >> parseArgs options rst

parseArgs options ("-id" : ide : rst) = parseArgs (options{_expId = ide}) rst
parseArgs options ("-i" : inputPath : rst) = parseArgs (options{_inputPath = inputPath}) rst
parseArgs options ("-t" : timeOut : rst) = parseArgs (options{_timeOut = read timeOut}) rst
parseArgs options ("-o" : outputPath : rst) = parseArgs (options{_outputPath = outputPath}) rst
parseArgs options ("-n" : numIterations : rst) = parseArgs (options{_iterations = read numIterations}) rst
parseArgs options ("-p" : "unsplitBias" : val : rst) = parseArgs (options{_unsplitBias = read val}) rst
parseArgs options ("-p" : "childBias" : val : rst) = parseArgs (options{_childBias = read val}) rst
parseArgs options [corpus, pieceName, algoName] = pure (corpus, pieceName, read algoName, options) -- concat `fmap` mapM readFile fs
parseArgs _ _ = usage >> exit

defaultInputPath = "preprocessing/inputs/"
defaultOutputPath = "preprocessing/outputs/"
defaultTimeOut = 1200
defaultNumIterations = 1
defaultUnsplitBias = 1
defaultChildBias = 1
defaultId = "000"

usage =
  putStrLn
    "\nUsage: parseFullPieces [-idvhio] corpus piece {RandomParse, RandomParseSBS, RandomSample, Heuristic1, HeuristicSBS1, All} \n\
    \   -v:         Verbose \n\
    \   -h:         Show Help \n\
    \   -i:         Set input path for chords and slices. Default: preprocessing/inputs/ \n\
    \   -id:        Set experiment id \n\
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
  Log.setLogLevel Log.LevelInfo
  (corpus, pieceName, algo,
    Options
     inputPath
     outputPath
     iterations
     timeOut
     unsplitBias
     childBias
     expId ) <-
    getArgs
      >>= parseArgs
        (Options
          defaultInputPath
          defaultOutputPath
          defaultNumIterations
          defaultTimeOut
          defaultUnsplitBias
          defaultChildBias
          defaultId)

  inputChords <- chordsFromFile (inputPath <> "chords/" <> corpus <> "/" <> pieceName <> ".csv")
  inputSlices <- slicesFromFile' (inputPath <> "slices/" <> corpus <> "/" <> pieceName <> ".csv")
  let outputFile = outputPath <> corpus <> "/" <> pieceName <> "/" <> showRoot algo <> "/" <> expId <> ".json"
  let outputFileDeriv = "testing3" -- outputPath <> corpus <> "/" <> pieceName <> "/" <> showRoot algo <> "/" <> expId
  createDirectoryIfMissing True $ outputPath <> corpus <> "/" <> pieceName <> "/" <> showRoot algo <> "/"

  res <- mapM (runAlgo unsplitBias childBias outputFileDeriv algo timeOut inputChords inputSlices numRetries) [1 .. iterations]

  writeJSONToFile outputFile $ concatResults (PieceResults expId (showRoot algo) corpus pieceName inputChords res)

  where
    numRetries = 3 :: Int

    runAlgo unsplitBias childBias deriv algo _ _ _ 0 id = pure $ nullResultToJSON algo
    runAlgo unsplitBias childBias deriv algo timeOut inputChords inputSlices n id = do
      mTimedRes <- case algo of
        StochasticSearch -> timeout (timeOut * 1000000) $ Time.timeItT $ runParse unsplitBias childBias algo (AlgoInputImpure protoVoiceEvaluatorImpure inputSlices inputChords)
        _ -> timeout (timeOut * 1000000) $ Time.timeItT $ runParse unsplitBias childBias algo (AlgoInputPure (protoVoiceEvaluatorLimitedSize 6 protoVoiceEvaluator) inputSlices inputChords)
      case mTimedRes of
        Nothing -> pure $ nullResultToJSON (show algo)
          -- runAlgo algo inputChords inputSlices (n - 1)
        Just (time, mRes) ->
          case mRes of
            Nothing -> runAlgo unsplitBias childBias deriv algo timeOut inputChords inputSlices (n - 1) id
            Just (AlgoResult top ops lbls) ->
              let accuracy = chordAccuracy inputChords lbls
                  likelihood = scoreSegments top inputChords
                in case ops of
                     Nothing -> let res = JsonResult top lbls ops accuracy likelihood (show algo) time (1 + numRetries - n) id Nothing
                                  in
                                    pure $ writeResultsToJSON res
                     Just (Analysis op to) -> do
                       plotDeriv (deriv) to op 
                       pure $ writeResultsToJSON (JsonResult top lbls ops accuracy likelihood (show algo) time (1 + numRetries - n) id Nothing)
                  -- logD $ "Accuracy: " <> show accuracy
                  -- logD $ "Likelihood: " <> show likelihood


plotDeriv fn top deriv = do
  case replayDerivation' top derivationPlayerPV deriv of
    (Left err) -> putStrLn err
    (Right g) -> viewGraph fn g
