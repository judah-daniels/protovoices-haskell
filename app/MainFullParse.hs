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
import PVGrammar.Parse (protoVoiceEvaluator, protoVoiceEvaluatorImpure)
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
import Algorithm (AlgoType(PerfectReduction))

data Options = Options
  { _inputPath :: String
  , _outputPath :: String
  , _iterations :: Int
  , _timeOut :: Int
  , _beamWidth :: BeamWidth
  , _unsplitWidth :: UnsplitWidth
  , _unspreadWidth :: UnspreadWidth
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
parseArgs options ("-p" : "beamWidth" : val : rst) = parseArgs (options{_beamWidth = read val}) rst
parseArgs options ("-p" : "unsplitWidth" : val : rst) = parseArgs (options{_unsplitWidth = read val}) rst
parseArgs options ("-p" : "unspreadWidth" : val : rst) = parseArgs (options{_unspreadWidth = read val}) rst
parseArgs options [corpus, pieceName, algoName] = pure (corpus, pieceName, read algoName, options) -- concat `fmap` mapM readFile fs
parseArgs _ _ = usage >> exit

defaultInputPath = "preprocessing/inputs/"
defaultOutputPath = "preprocessing/outputs/"
defaultTimeOut = 1200
defaultNumIterations = 1
defaultUnspreadWidth = 7
defaultUnsplitWidth = 3
defaultBeamWidth = 10
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
     beamWidth
     unsplitWidth
     unSpreadWidth
     expId ) <-
    getArgs
      >>= parseArgs
        (Options
          defaultInputPath
          defaultOutputPath
          defaultNumIterations
          defaultTimeOut
          defaultBeamWidth
          defaultUnsplitWidth
          defaultUnspreadWidth
          defaultId)

  inputChords <- chordsFromFile (inputPath <> "chords/" <> corpus <> "/" <> pieceName <> ".csv")
  inputSlices <- slicesFromFile' (inputPath <> "slices/" <> corpus <> "/" <> pieceName <> ".csv")
  let outputFile = outputPath <> corpus <> "/" <> pieceName <> "/" <> showRoot algo <> "/" <> expId <> ".json"
  let outputFileDeriv = outputPath <> corpus <> "/" <> pieceName <> "/" <> showRoot algo <> "/" <> expId
  createDirectoryIfMissing True $ outputPath <> corpus <> "/" <> pieceName <> "/" <> showRoot algo <> "/"

  res <- replicateM iterations $ runAlgo outputFileDeriv algo timeOut inputChords inputSlices numRetries

  writeJSONToFile outputFile $ concatResults expId (showRoot algo) corpus pieceName inputChords res

  where
    numRetries = 3 :: Int

    -- findBeam algo _ _ _ 0 = pure $ nullResultToJSON algo
    -- findBeam algo timeOut inputChords inputSlices n = do
    --   mTimedRes <- timeout (timeOut * 1000000) $ Time.timeItT $ runParse algo (AlgoInput protoVoiceEvaluator inputSlices inputChords)
    --   case mTimedRes of
    --     Nothing -> pure $ nullResultToJSON (show algo)
    --       -- runAlgo algo inputChords inputSlices (n - 1)
    --     Just (time, mRes) ->
    --       case mRes of
    --         Nothing -> runAlgo expId algo timeOut inputChords inputSlices (n - 1)
    --         Just (AlgoResult top ops lbls) ->
    --           let accuracy = chordAccuracy inputChords lbls
    --               likelihood = scoreSegments top lbls
    --             in do
    --               logD $ "Accuracy: " <> show accuracy
    --               logD $ "Likelihood: " <> show likelihood
    --
    --               pure $ writeResultsToJSON top lbls ops accuracy likelihood (show algo) time (1 + numRetries - n)

    runAlgo deriv algo _ _ _ 0 = pure $ nullResultToJSON algo
    runAlgo deriv algo timeOut inputChords inputSlices n = do
      mTimedRes <- case algo of 
        StochasticSearch -> timeout (timeOut * 1000000) $ Time.timeItT $ runParse algo (AlgoInputImpure protoVoiceEvaluatorImpure inputSlices inputChords)
        _ -> timeout (timeOut * 1000000) $ Time.timeItT $ runParse algo (AlgoInputPure protoVoiceEvaluator inputSlices inputChords)
      case mTimedRes of
        Nothing -> pure $ nullResultToJSON (show algo)
          -- runAlgo algo inputChords inputSlices (n - 1)
        Just (time, mRes) ->
          case mRes of
            Nothing -> runAlgo deriv algo timeOut inputChords inputSlices (n - 1)
            Just (AlgoResult top ops lbls) ->
              let accuracy = chordAccuracy inputChords lbls
                  likelihood = scoreSegments top lbls
                in case ops of 
                     Nothing -> pure $ writeResultsToJSON top lbls ops accuracy likelihood (show algo) time (1 + numRetries - n)
                     Just (Analysis op to) -> do 
                       -- plotDeriv (deriv) to op 
                       pure $ writeResultsToJSON top lbls ops accuracy likelihood (show algo) time (1 + numRetries - n)
                  -- logD $ "Accuracy: " <> show accuracy
                  -- logD $ "Likelihood: " <> show likelihood


    showRoot algo =
      case algo of
        BeamSearch width -> "BeamSearch_" <> show width
        StochasticBeamSearch width res -> "StochasticBeamSearch_" <> show width <> "_" <> show res
        StochasticBeamSearchLimited width res n-> "StochasticBeamSearchLimited_" <> show width <> "_" <> show res <> "_" <> show n
        DualStochasticBeamSearch width res -> "DualStochasticBeamSearch_" <> show width <> "_" <> show res 
        DualBeamSearch a b -> "DualBeamSearch_" <> show a <> "_" <> show b
        BeamSearchPerSegment width -> "BeamSearchPerSegment_" <> show width 
        -- PerfectReduction threshold -> "BeamSearchPerSegment_" <> show threshold 
        _ -> show algo
  -- = RandomWalk
  -- | RandomWalkPerSegment
  -- | RandomSample
  -- | RandomReduction
  -- | BeamSearch BeamWidth
  -- | StochasticBeamSearch BeamWidth ResevoirSize
  -- | DualStochasticBeamSearch BeamWidth ResevoirSize
  -- | StochasticBeamSearchLimited BeamWidth ResevoirSize MaxNotesPerSlice
  -- | BeamSearchPerSegment BeamWidth
  -- | DualBeamSearch UnspreadWidth UnsplitWidth

plotDeriv fn top deriv = do
  case replayDerivation' top derivationPlayerPV deriv of
    (Left err) -> putStrLn err
    (Right g) -> viewGraph fn g
