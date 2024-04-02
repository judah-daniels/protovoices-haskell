{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm
  (
    AlgoInput (..)
  , ParseAlgo (..)
  , AlgoResult (..)
  , AlgoType (..)
  , showRoot
  , UnsplitBias
  , ChildBias
  , BeamWidth
  )
  where

import Control.Logging qualified as Log
import Data.Text qualified as T

import FileHandling ( InputSlice (..), pathFromSlices, splitSlicesIntoSegments)

import Heuristics
import Parser.HeuristicParser

import Harmony
import Harmony.ChordLabel
import Harmony.Params

import Algorithm.RandomChoiceSearch
import Algorithm.RandomSampleParser
import Algorithm.HeuristicSearch
import Algorithm.InformedReduction
import Algorithm.Templating

import Data.Maybe
import qualified Internal.MultiSet as MS
import Control.Monad.Except

import Common
import PVGrammar

import Musicology.Core
import PVGrammar.Parse


data AlgoInput
  = AlgoInputPure
      (Eval (Edges SPitch) [Edge SPitch] (Notes SPitch) [SPitch] (PVLeftmost SPitch))
      [InputSlice SPitch]
      [ChordLabel]
  | AlgoInputImpure
      (EvalImpure (Edges SPitch) [Edge SPitch] (Notes SPitch) [SPitch] (PVLeftmost SPitch))
      [InputSlice SPitch]
      [ChordLabel]


type ReductionAnalysis = [Notes SPitch]

data AlgoResult
  = BlackBoxResult [ChordLabel]
  | ReductionResult ReductionAnalysis
  | PVResult (PVAnalysis SPitch)
  deriving (Show)

class ParseAlgo algo where
  runParse :: Double -> Double -> algo -> AlgoInput -> IO (Maybe AlgoResult)

type BeamWidth = Int
type Threshold = Double
type ChildBias = Double
type UnsplitBias = Double
type ResevoirSize = Int
type MaxNotesPerSlice = Int

data AlgoType
  = RandomWalk
  | RandomWalkPerSegment
  | RandomSample
  | RandomReduction
  | PerfectReduction
  | BeamSearch BeamWidth
  | StochasticBeamSearch BeamWidth ResevoirSize
  | DualStochasticBeamSearch BeamWidth ResevoirSize
  | DualStochasticBeamSearch' BeamWidth ResevoirSize UnsplitBias ChildBias
  | StochasticBeamSearchLimited BeamWidth ResevoirSize MaxNotesPerSlice
  | BeamSearchPerSegment BeamWidth
  | StochasticSearch
  | Templating
  deriving (Show, Read, Eq)

showRoot algo =
  case algo of
    BeamSearch width -> "BeamSearch_" <> show width
    StochasticBeamSearch width res -> "StochasticBeamSearch_" <> show width <> "_" <> show res
    StochasticBeamSearchLimited width res n-> "StochasticBeamSearchLimited_" <> show width <> "_" <> show res <> "_" <> show n
    DualStochasticBeamSearch width res -> "DualStochasticBeamSearch_" <> show width <> "_" <> show res
    DualStochasticBeamSearch' width res a b -> "DualStochasticBeamSearch_" <> show width <> "_" <> show res <> "_" <> show a <> "_" <> show b
    BeamSearchPerSegment width -> "BeamSearchPerSegment_" <> show width
    -- DualBeamSearch a b -> "DualBeamSearch_" <> show a <> "_" <> show b
    -- PerfectReduction threshold -> "BeamSearchPerSegment_" <> show threshold 
    _ -> show algo

instance ParseAlgo AlgoType where
  runParse unsplitBias childBias algoType (AlgoInputImpure eval'@(EvalImpure eval evalUnsplitImpure) inputSlices chords) = case algoType of
    StochasticSearch ->
      let initialState = SSFrozen $ pathFromSlices eval sliceWrapper inputSlices
       in
        Log.timedLog "Running Heuristic Search" $ do
          res <- runExceptT
            (stochasticSearch
              10
              initialState
              (exploreStates' sliceWrapper eval')
              (goalTest chords)
              (applyHeuristic (heuristicZero unsplitBias childBias))
            )

          case res of
            Left err -> do
              Log.warn $ T.pack err
              pure Nothing
            Right finalState ->
              let p = fromJust $ getPathFromState finalState
                  ops = getOpsFromState finalState
                  slices = pathBetweens p
                  chordGuesses = guessChords  slices
               in
               pure $ Just $ PVResult (Analysis ops p)

  runParse unsplitBias childBias algoType (AlgoInputPure eval inputSlices chords) = case algoType of
    DualStochasticBeamSearch' beamWidth resevoirSize unsplitBias childBias ->
      let initialState = SSFrozen $ pathFromSlices eval sliceWrapper inputSlices
       in
        Log.timedLog "Running Heuristic Search" $ do
          res <- runExceptT
            (dualStochasticBeamSearch
              beamWidth
              resevoirSize
              initialState
              (exploreStates sliceWrapper eval)
              (goalTest chords)
              (applyHeuristic (heuristicZero unsplitBias childBias))
            )

          case res of
            Left err -> do
              Log.warn $ T.pack err
              pure Nothing
            Right finalState ->
              let p = fromJust $ getPathFromState finalState
                  ops = getOpsFromState finalState
                  slices = pathBetweens p
                  chordGuesses = guessChords  slices
               in
               pure $ Just $ PVResult (Analysis ops p)

    DualStochasticBeamSearch beamWidth resevoirSize ->
      let initialState = SSFrozen $ pathFromSlices eval sliceWrapper inputSlices
       in
        Log.timedLog "Running Heuristic Search" $ do
          res <- runExceptT
            (dualStochasticBeamSearch
              beamWidth
              resevoirSize
              initialState
              (exploreStates sliceWrapper eval)
              (goalTest chords)
              (applyHeuristic (heuristicZero unsplitBias childBias))
            )

          case res of
            Left err -> do
              Log.warn $ T.pack err
              pure Nothing
            Right finalState ->
              let p = fromJust $ getPathFromState finalState
                  ops = getOpsFromState finalState
                  slices = pathBetweens p
                  chordGuesses = guessChords  slices
               in
               pure $ Just $ PVResult (Analysis ops p) 

    RandomWalk ->
      let initialState = SSFrozen $ pathFromSlices eval idWrapper inputSlices
       in
        do
          res <- runExceptT
            (randomChoiceSearch initialState (exploreStates idWrapper eval) (goalTest chords) (showOp . getOpsFromState))
          case res of
            Left err -> do
              print err
              pure Nothing
            Right finalState ->
              let path = fromMaybe (error "failed to get path from state") $ getPathFromState finalState
                  ops = getOpsFromState finalState
                  slices = pathBetweens path
                  chordGuesses = guessChords slices
               in
               pure $ Just $ PVResult (Analysis ops path)

    RandomWalkPerSegment ->
      let initialState = SSFrozen $ pathFromSlices eval idWrapper inputSlices
       in do
          res <- runExceptT
            (randomChoiceSearch initialState (exploreStates idWrapper eval) (goalTest chords) (showOp . getOpsFromState))
          case res of
            Left err -> do
              print err
              pure Nothing
            Right finalState ->
              let path = fromMaybe (error "failed to get path from state") $ getPathFromState finalState
                  ops = getOpsFromState finalState
                  slices = pathBetweens path
                  chordGuesses = guessChords slices
               in
               pure $ Just $ PVResult (Analysis ops path)

    RandomSample -> do
        path <- randomSamplePath (length chords)
        let slices = pathBetweens path
            chordGuesses = guessChords slices
         in pure $ Just (ReductionResult slices)

    PerfectReduction ->
      let x = splitSlicesIntoSegments eval sliceWrapper inputSlices
          slices = informedReduction x chords
          chordGuesses = guessChords slices
        in pure $ Just (ReductionResult slices )

    Templating ->
      let x = splitSlicesIntoSegments eval sliceWrapper inputSlices
       in Log.timedLog "Running Templating Baseline" $ do
        let (slices, chordGuesses)  = templatingBaseline x
         in pure $ Just $ ReductionResult slices

    RandomReduction ->
      let x = splitSlicesIntoSegments eval sliceWrapper inputSlices
       in Log.timedLog "Running Random Sample SBS Parse" $ do
        slices <- randomSamplePathSBS x
        let chordGuesses = guessChords slices
         in pure $ Just $ ReductionResult slices

    -- StochasticBeamSearch beamWidth resevoirSize ->
    --   let initialState = SSFrozen $ pathFromSlices eval sliceWrapper inputSlices
    --    in
    --     Log.timedLog "Running Heuristic Search" $ do
    --       res <- runExceptT
    --         (stochasticBeamSearch
    --           beamWidth
    --           resevoirSize
    --           initialState
    --           (exploreStates sliceWrapper eval)
    --           (goalTest chords)
    --           (applyHeuristic heuristicZero)
    --         )
    --
    --       case res of
    --         Left err -> do
    --           Log.warn $ T.pack err
    --           pure Nothing
    --         Right finalState ->
    --           let p = fromJust $ getPathFromState finalState
    --               ops = getOpsFromState finalState
    --               slices = pathBetweens p
    --               chordGuesses = guessChords  slices
    --            in
    --            pure $ Just $ AlgoResult slices (Just (Analysis ops p)) chordGuesses
    --
    -- StochasticBeamSearchLimited beamWidth resevoirSize maxNotesPerSlice ->
    --   let initialState = SSFrozen $ pathFromSlices eval sliceWrapper inputSlices
    --    in
    --     Log.timedLog "Running Heuristic Search" $ do
    --       res <- runExceptT
    --         (stochasticBeamSearchLimited
    --           beamWidth
    --           resevoirSize
    --           initialState
    --           (exploreStates sliceWrapper (protoVoiceEvaluatorLimitedSize maxNotesPerSlice eval))
    --           (goalTest chords)
    --           (applyHeuristic heuristicZero)
    --         )
    --
    --       case res of
    --         Left err -> do
    --           Log.warn $ T.pack err
    --           pure Nothing
    --         Right finalState ->
    --           let p = fromJust $ getPathFromState finalState
    --               ops = getOpsFromState finalState
    --               slices = pathBetweens p
    --               chordGuesses = guessChords  slices
    --            in
    --            pure $ Just $ AlgoResult slices (Just (Analysis ops p)) chordGuesses
    --
    -- BeamSearch beamWidth ->
    --   let initialState = SSFrozen $ pathFromSlices eval sliceWrapper inputSlices
    --    in
    --     Log.timedLog "Running Heuristic Search" $ do
    --       res <- runExceptT
    --         (beamSearch
    --           beamWidth
    --           initialState
    --           (exploreStates sliceWrapper eval)
    --           (goalTest chords)
    --           (applyHeuristic heuristicZero)
    --         )
    --
    --       case res of
    --         Left err -> do
    --           Log.warn $ T.pack err
    --           pure Nothing
    --         Right finalState ->
    --           let p = fromJust $ getPathFromState finalState
    --               ops = getOpsFromState finalState
    --               slices = pathBetweens p
    --               chordGuesses = guessChords  slices
    --            in
    --            pure $ Just $ AlgoResult slices (Just (Analysis ops p)) chordGuesses
