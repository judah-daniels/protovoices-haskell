{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import Data.Maybe
  ( catMaybes,
    isNothing,
    fromMaybe,
    fromJust,
    mapMaybe,
    maybeToList,
  )
import FileHandling
import Data.Hashable
import HeuristicParser
import HeuristicSearch
import Control.Monad.Except (ExceptT,runExceptT, lift, throwError)

-- import Language.Haskell.DoNotation
import Musicology.Core
import Heuristics
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled
import PBHModel
import PVGrammar
import PVGrammar.Parse
import RandomChoiceSearch
import RandomSampleParser
import Test.Hspec

import System.Environment
import System.Exit

-- COMAND LINE ARGUMENT HANDLING
parseArgs ["-h"] = usage >> exit
parseArgs ["-v"] = version >> exit
parseArgs [chordsFile, slicesFile, jsonFile] = pure (chordsFile, slicesFile, jsonFile) -- concat `fmap` mapM readFile fs
parseArgs _ = usage >> exit

usage = putStrLn "Usage: parseFullPieces [-vh] chordsFile slicesFile jsonFile"
version = putStrLn "Version 0.1"
exit = exitSuccess
die = exitWith (ExitFailure 1)

-- Run 3 search algorithms on the inputs given
main :: IO ()
main = do
  (chordsFile, slicesFile, jsonFile) <- getArgs >>= parseArgs
  params <- loadParams "preprocessing/dcml_params.json"
  chords <- chordsFromFile chordsFile
  -- print chordsFile
  slices <- slicesFromFile' slicesFile
  let wrap = SliceWrapper $ \ns -> let (r, l, p) = mostLikelyChordFromSlice params ns in SliceWrapped ns (ChordLabel l r) p

  scores <-
    evaluateSearches
      [ ("Heuristic", runHeuristicSearch params protoVoiceEvaluator wrap (applyHeuristic (testHeuristic params)))
      , ("RandomParse", runRandomSearch params (protoVoiceEvaluatorLimitedSize 10))
      , ("RandomSample", runRandomSampleSearch)
      ]
      (scoreSegments params (scoreSegment' params))
      slices
      chords
  -- let scores = [("Heuristic", 3),("RandomSample", 4),("RandomParse", 7)]
  print jsonFile

  writeMapToJson scores jsonFile

  -- mapM_ print scores

evaluateSearches
  :: forall bs
   . [(String, [InputSlice SPitch] -> [ChordLabel] -> IO (Path bs (Notes SPitch)))]
  -> ([Notes SPitch] -> [ChordLabel] -> Double)
  -> [InputSlice SPitch]
  -> [ChordLabel]
  -> IO [(String, Double)]
evaluateSearches algos scorer inputSlices chordLabels =
  mapM
    ( \(algoName, algo) -> do
        resultingPath <- algo inputSlices chordLabels
        let slices = pathBetweens resultingPath
        pure (algoName, scorer slices chordLabels)
    )
    algos

evaluateSearch
  :: ([InputSlice SPitch] -> [ChordLabel] -> IO (Path (Edges SPitch) (Notes SPitch)))
  -> ([Notes SPitch] -> [ChordLabel] -> Double)
  -> [InputSlice SPitch]
  -> [ChordLabel]
  -> IO Double
evaluateSearch algo scorer inputSlices chordLabels = do
  resultingPath <- algo inputSlices chordLabels
  let slices = pathBetweens resultingPath
  let score = scorer slices chordLabels
  pure score

runRandomSampleSearch
  :: forall bs
   . bs
  -> [ChordLabel]
  -> IO (Path (Edges SPitch) (Notes SPitch))
runRandomSampleSearch _ chordLabels = do
  randomSamplePath (length chordLabels)

runRandomSearch
  :: ( Music.HasPitch ns
     , Eq (Music.IntervalOf ns)
     , Data.Hashable.Hashable ns
     , Ord ns
     , Show ns
     , Music.Notation ns
     )
  => HarmonicProfileData
  -> Eval (Edges ns) [Edge ns] (Notes ns) [ns] (PVLeftmost ns)
  -> [InputSlice ns]
  -> [ChordLabel]
  -> IO (Path (Edges ns) (Notes ns))
runRandomSearch params eval inputSlices chordLabels = do
  let initialState = SSFrozen $ pathFromSlices eval idWrapper inputSlices
  res <- runExceptT (randomChoiceSearch initialState getNeighboringStates goalTest (showOp . getOpsFromState))
  finalState <- case res of
    Left err -> do
      print err
      return undefined
    Right s -> pure s

  let p = fromMaybe undefined $ getPathFromState finalState
  let ops = getOpsFromState finalState

  pure p
 where
  showOp [] = ""
  showOp (x : _) = case x of
    LMDouble y -> show y
    LMSingle y -> show y

  getNeighboringStates = exploreStates idWrapper eval

  -- The goal is to find a state with a slice for each chord label.
  goalTest (SSOpen p _) = pathLen p - 1 == length chordLabels
  goalTest _ = False

-----
runHeuristicSearch
  :: ( Music.HasPitch ns
     , Eq (Music.IntervalOf ns)
     , Data.Hashable.Hashable ns
     , Ord ns
     , Show ns
     , Music.Notation ns
     )
  => HarmonicProfileData
  -> Eval (Edges ns) [Edge ns] (Notes ns) [ns] (PVLeftmost ns)
  -> SliceWrapper (Notes ns)
  -> ((Maybe (State ns), State ns) -> ExceptT String IO Double)
  -> [InputSlice ns]
  -> [ChordLabel]
  -> IO (Path (Edges ns) (Notes ns))
runHeuristicSearch params eval wrap heuristic inputSlices chordLabels = do
  let initialState = SSFrozen $ pathFromSlices eval wrap inputSlices
  res <- runExceptT (heuristicSearch initialState getNeighboringStates goalTest heuristic (showOp . getOpsFromState))
  finalState <- case res of
    Left err -> do
      print err
      print "something went wrong"
      return undefined
    Right s -> pure s

  let p = fromJust $ getPathFromState finalState
  print p

  -- Chord Guesses for evaluation with other model
  let chordGuesses = sLbl <$> pathBetweens (fromJust $ getPathFromState' finalState)
  mapM_ (\(ChordLabel lbl root) -> putStrLn $ showNotation root <> lbl) chordGuesses

  let ops = getOpsFromState finalState

  -- pure (p, ops)
  pure p
 where
  showOp [] = ""
  showOp (x : _) = case x of
    LMDouble y -> show y
    LMSingle y -> show y

  getNeighboringStates = exploreStates wrap eval

  -- The goal is to find a state with a slice for each chord label.
  goalTest (SSOpen p _) = pathLen p - 1 == length chordLabels
  goalTest _ = False
