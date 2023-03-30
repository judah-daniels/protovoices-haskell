{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import Control.Monad.Except (ExceptT, forM, lift, runExceptT, throwError, when, zipWithM)
import Data.Hashable
import Data.Maybe
  ( catMaybes
  , fromJust
  , fromMaybe
  , isNothing
  , mapMaybe
  , maybeToList
  )
import System.TimeIt qualified as Time
import System.Timeout

-- LOGGING

import Control.Logging qualified as Log
import Data.Text qualified as T

import FileHandling
import HeuristicParser
import Heuristics
import PBHModel

import Core
import Musicology.Core
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled
import PVGrammar
import PVGrammar.Parse
import Test.Hspec

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as A
import System.Environment
import System.Exit

data Options = Options
  { _inputPath :: String
  , _outputPath :: String
  , _iterations :: Int
  }

-- COMAND LINE ARGUMENT HANDLING
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

main :: IO ()
main = Log.withStdoutLogging $ do
  params <- loadParams "preprocessing/dcml_params.json"
  (corpus, pieceName, algo, Options inputPath outputPath iterations) <-
    getArgs
      >>= parseArgs (Options defaultInputPath defaultOutputPath defaultNumIterations)

  let outputFile = outputPath <> corpus <> "/" <> pieceName <> ".json"
  inputChords <- chordsFromFile (inputPath <> "chords/" <> corpus <> "/" <> pieceName <> ".csv")
  inputSlices <- slicesFromFile' (inputPath <> "slices/" <> corpus <> "/" <> pieceName <> ".csv")

  let scorer = scoreSegments params scoreSegment'

  res <- case algo of
    -- Replicate all searches but not the heuristic searches as they are (currently) deterministic
    All ->
      forM
        ( concatMap
            (replicate iterations)
            [RandomParse, RandomParseSBS, RandomSampleSBS, RandomSample]
            <> [Heuristic1, HeuristicSBS1]
        )
        ( \a -> do
            m <- timeout timeOutMs $ Time.timeItT $ runAlgo a scorer params inputChords inputSlices
            case m of
              Nothing -> nullResultToJSON a
              Just (time, r) -> resultToJSON a time r
        )
    _ ->
      forM
        [1 .. iterations]
        ( \_ -> do
            m <- timeout timeOutMs $ Time.timeItT $ runAlgo algo scorer params inputChords inputSlices
            case m of
              Nothing -> nullResultToJSON algo
              Just (time, r) -> resultToJSON algo time r
        )

  writeJSONToFile outputFile $ concatResults corpus pieceName inputChords res
