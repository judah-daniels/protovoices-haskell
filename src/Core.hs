{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core where

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

import Control.Logging qualified as Log
import Data.Text qualified as T

import Algorithm.HeuristicSearch
import Algorithm.RandomChoiceSearch
import Algorithm.RandomSampleParser
import FileHandling
import HeuristicParser
import Heuristics
import PBHModel

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

data ParseAlgo
  = RandomParse
  | RandomParseSBS
  | RandomSample
  | RandomSampleSBS
  | Heuristic1
  | HeuristicSBS1
  | All
  deriving (Read, Show, Eq)

data AlgoResult = AlgoResult [Notes SPitch] [ChordLabel] (Maybe (Path (Edges SPitch) (Notes SPitch))) Double Double
  deriving (Show)

--   Main entry point

timeOutMs = 400 * 1000000 :: Int

{- | Runs a random search through the entire piece
     This can get stuck due to combinatoral blowup.
     This can also reach a deadend
-}
runRandomParse
  :: Eval (Edges SPitch) [Edge SPitch] (Notes SPitch) [SPitch] (PVLeftmost SPitch)
  -> ([Notes SPitch] -> [ChordLabel] -> Double)
  -> HarmonicProfileData
  -> [ChordLabel]
  -> [InputSlice SPitch]
  -> IO AlgoResult
runRandomParse eval scorer params chords inputSlices =
  let
    initialState = SSFrozen $ pathFromSlices eval idWrapper inputSlices
   in
    do
      res <-
        runExceptT
          (randomChoiceSearch initialState (exploreStates idWrapper eval) (goalTest chords) (showOp . getOpsFromState))

      finalState <- case res of
        Left err -> print err >>= return undefined
        Right s -> pure s

      let path = fromJust $ getPathFromState finalState
          slices = pathBetweens path
          chordGuesses = guessChords params slices
          likelihood = scorer slices chords
          accuracy = chordAccuracy chords chordGuesses
       in pure $ AlgoResult slices chordGuesses Nothing accuracy likelihood

runRandomParseSingleSegment
  :: HarmonicProfileData
  -> Eval (Edges SPitch) [Edge SPitch] (Notes SPitch) [SPitch] (PVLeftmost SPitch)
  -> SliceWrapper (Notes SPitch)
  -> [InputSlice SPitch]
  -> ChordLabel
  -> IO (SliceWrapped (Notes SPitch))
runRandomParseSingleSegment params eval wrap inputSlices chordLabel = do
  let initialState = SSFrozen $ pathFromSlices eval wrap inputSlices
  res <- runExceptT (randomChoiceSearch initialState (exploreStates wrap eval) goalTestSBS (showOp . getOpsFromState))
  finalState <- case res of
    Left err -> do
      Log.errorL $ T.pack err
    Right s -> pure s

  let p = fromJust $ getPathFromState' finalState
  let finalSlice = case pathBetweens p of
        [finalSlice] -> finalSlice
        _ -> Log.errorL "Run random Parse single Segment: Single slice not returned by heuristic search"
  pure finalSlice

-- | Runs a random search within each segment
runRandomParseSBS
  :: Eval (Edges SPitch) [Edge SPitch] (Notes SPitch) [SPitch] (PVLeftmost SPitch)
  -> ([Notes SPitch] -> [ChordLabel] -> Double)
  -> HarmonicProfileData
  -> [ChordLabel]
  -> [InputSlice SPitch]
  -> IO AlgoResult
runRandomParseSBS eval scorer params chords inputSlices =
  let x = splitSlicesIntoSegments eval (sliceWrapper params) inputSlices
   in Log.timedLog "Running Random Parse SBS" $ do
        res <- zipWithM (runRandomParseSingleSegment params eval idWrapper) x chords

        let slices = sWContent <$> res
            chordGuesses = guessChords params slices
            likelihood = scorer slices chords
            accuracy = chordAccuracy chords chordGuesses
         in pure $ AlgoResult slices chordGuesses Nothing accuracy likelihood

-- | Samples random notes for every segment, without looking at the segment itself
runRandomSample
  :: Eval (Edges SPitch) [Edge SPitch] (Notes SPitch) [SPitch] (PVLeftmost SPitch)
  -> ([Notes SPitch] -> [ChordLabel] -> Double)
  -> HarmonicProfileData
  -> [ChordLabel]
  -> [InputSlice SPitch]
  -> IO AlgoResult
runRandomSample eval scorer params chords inputSlices =
  let x = splitSlicesIntoSegments eval (sliceWrapper params) inputSlices
   in Log.timedLog "Running Random Sample Parse" $ do
        path <- randomSamplePath (length chords)
        let slices = pathBetweens path
            chordGuesses = guessChords params slices
            likelihood = scorer slices chords
            accuracy = chordAccuracy chords chordGuesses
         in pure $ AlgoResult slices chordGuesses Nothing accuracy likelihood

-- | Samples random notes from each segment
runRandomSampleSBS
  :: Eval (Edges SPitch) [Edge SPitch] (Notes SPitch) [SPitch] (PVLeftmost SPitch)
  -> ([Notes SPitch] -> [ChordLabel] -> Double)
  -> HarmonicProfileData
  -> [ChordLabel]
  -> [InputSlice SPitch]
  -> IO AlgoResult
runRandomSampleSBS eval scorer params chords inputSlices =
  let x = splitSlicesIntoSegments eval (sliceWrapper params) inputSlices
   in Log.timedLog "Running Random Sample SBS Parse" $ do
        path <- randomSamplePathSBS x

        let slices = pathBetweens path
            chordGuesses = guessChords params slices
            likelihood = scorer slices chords
            accuracy = chordAccuracy chords chordGuesses
         in pure $ AlgoResult slices chordGuesses Nothing accuracy likelihood

-- {- | Uses a beam search, using chordtone and ornamentation probabilities as a score
-- -}
-- runHeuristic1 :: HarmonicProfileData -> [ChordLabel] -> [InputSlice SPitch] -> String -> IO ()
-- runHeuristic1 params chordsFile slicesFile jsonFile = do
--   pure ()
--

-- | Uses a beam search, using chordtone and ornamentation probabilities as a score, but running separately for each segment
runHeuristicSBS1
  :: Eval (Edges SPitch) [Edge SPitch] (Notes SPitch) [SPitch] (PVLeftmost SPitch)
  -> ([Notes SPitch] -> [ChordLabel] -> Double)
  -> HarmonicProfileData
  -> [ChordLabel]
  -> [InputSlice SPitch]
  -> IO AlgoResult
runHeuristicSBS1 eval scorer params chords inputSlices =
  let x = splitSlicesIntoSegments eval (sliceWrapper params) inputSlices
   in Log.timedLog "Running Heuristic Search 1 SBS" $ do
        resultingSlices <- zipWithM (runHeuristicSearchSingleSegment params eval (sliceWrapper params) (testHeuristic params)) x chords

        let chordGuesses = sLbl <$> resultingSlices
            slices = sWContent <$> resultingSlices
            likelihood = scorer slices chords
            accuracy = chordAccuracy chords chordGuesses
         in pure $ AlgoResult slices chordGuesses Nothing accuracy likelihood

-----
runHeuristic1
  :: Eval (Edges SPitch) [Edge SPitch] (Notes SPitch) [SPitch] (PVLeftmost SPitch)
  -> ([Notes SPitch] -> [ChordLabel] -> Double)
  -> HarmonicProfileData
  -> [ChordLabel]
  -> [InputSlice SPitch]
  -> IO AlgoResult
runHeuristic1 eval scorer params chords inputSlices = Log.timedLog "Running Heuristic Search" $ do
  let initialState = SSFrozen $ pathFromSlices eval (sliceWrapper params) inputSlices
  res <- runExceptT (heuristicSearch initialState (exploreStates (sliceWrapper params) eval) (goalTest chords) (applyHeuristic (testHeuristic params)) (showOp . getOpsFromState))

  finalState <- case res of
    Left err -> do
      Log.warn $ T.pack err
      return undefined
    Right s -> pure s

  let resultingSlices = pathBetweens $ fromJust $ getPathFromState' finalState
      chordGuesses = sLbl <$> resultingSlices
      slices = sWContent <$> resultingSlices
      likelihood = scorer slices chords
      accuracy = chordAccuracy chords chordGuesses
   in pure $ AlgoResult slices chordGuesses Nothing accuracy likelihood

runHeuristicSearch
  :: HarmonicProfileData
  -> Eval (Edges SPitch) [Edge SPitch] (Notes SPitch) [SPitch] (PVLeftmost SPitch)
  -> SliceWrapper (Notes SPitch)
  -> ((Maybe (State SPitch), State SPitch) -> ExceptT String IO Double)
  -> [InputSlice SPitch]
  -> [ChordLabel]
  -> IO (Path (Edges SPitch) (Notes SPitch))
runHeuristicSearch params eval wrap heuristic inputSlices chordLabels = do
  let initialState = SSFrozen $ pathFromSlices eval wrap inputSlices
  res <- runExceptT (heuristicSearch initialState getNeighboringStates (goalTest chordLabels) heuristic (showOp . getOpsFromState))
  finalState <- case res of
    Left err -> do
      Log.warn $ T.pack err
      return undefined
    Right s -> pure s

  let p = fromJust $ getPathFromState finalState
  print p

  -- Chord Guesses for evaluation with other model
  let chordGuesses = sLbl <$> pathBetweens (fromJust $ getPathFromState' finalState)
  mapM_ (\(ChordLabel lbl root) -> putStrLn $ showNotation root <> lbl) chordGuesses

  let ops = getOpsFromState finalState

  pure p
 where
  showOp [] = ""
  showOp (x : _) = case x of
    LMDouble y -> show y
    LMSingle y -> show y

  getNeighboringStates = exploreStates wrap eval

runHeuristicSearchSingleSegment
  :: HarmonicProfileData
  -> Eval (Edges SPitch) [Edge SPitch] (Notes SPitch) [SPitch] (PVLeftmost SPitch)
  -> SliceWrapper (Notes SPitch)
  -> ((Maybe (State SPitch), State SPitch) -> ExceptT String IO Double)
  -> [InputSlice SPitch]
  -> ChordLabel
  -> IO (SliceWrapped (Notes SPitch))
runHeuristicSearchSingleSegment params eval wrap heuristic inputSlices chordLabel = do
  let initialState = SSFrozen $ pathFromSlices eval wrap inputSlices
  res <- runExceptT (heuristicSearch initialState (exploreStates wrap eval) goalTestSBS heuristic (showOp . getOpsFromState))
  finalState <- case res of
    Left err -> do
      Log.errorL $ T.pack err
    Right s -> pure s

  let p = fromJust $ getPathFromState' finalState
      finalSlice = case pathBetweens p of
        [finalSlice] -> finalSlice
        _ -> Log.errorL "runHeuristicSearchSingleSegment: Single slice not returned by heuristic search"
   in pure finalSlice

resultToJSON :: ParseAlgo -> Double -> AlgoResult -> IO A.Value
resultToJSON a time (AlgoResult sl ch pa ac li) =
  pure $ writeResultsToJSON sl ch pa ac li (show a) time

runAlgo algo scorer params inputChords inputSlices =
  let run = case algo of
        RandomParse -> runRandomParse
        RandomParseSBS -> runRandomParseSBS
        RandomSample -> runRandomSample
        RandomSampleSBS -> runRandomSampleSBS
        Heuristic1 -> runHeuristic1
        HeuristicSBS1 -> runHeuristicSBS1
        All -> error ""
   in run protoVoiceEvaluator scorer params inputChords inputSlices
