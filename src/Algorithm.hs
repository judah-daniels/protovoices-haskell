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
  , UnsplitWidth
  , UnspreadWidth
  , BeamWidth
  )
  where

import Control.Logging qualified as Log
import Data.Text qualified as T

import FileHandling ( InputSlice (..), pathFromSlices, splitSlicesIntoSegments)

import Harmony
import Harmony.ChordLabel
import Harmony.Params

import Algorithm.RandomChoiceSearch
import Algorithm.RandomSampleParser
import Algorithm.HeuristicSearch

import Common ( Leftmost(..) , LeftmostDouble(..) , LeftmostSingle(..), Eval (Eval), pathBetweens)
import PVGrammar ( Edge, Edges, Freeze, Notes (Notes), Split, Spread, PVLeftmost )

import Control.Monad.Except (ExceptT, lift, throwError)
import Data.Maybe (fromMaybe, fromJust)

import HeuristicParser 
import Musicology.Core ( SPitch, showNotation )
import Control.Monad.Trans.Except (runExceptT)
import Heuristics
import PVGrammar.Parse (protoVoiceEvaluatorLimitedSize)
import qualified Internal.MultiSet as MS

data AlgoInput =
  AlgoInput
  (Eval (Edges SPitch) [Edge SPitch] (Notes SPitch) [SPitch] (PVLeftmost SPitch))
  [InputSlice SPitch]
  [ChordLabel]

data AlgoResult = AlgoResult
  { arTop :: [Notes SPitch]
  , arOps :: Maybe [PVLeftmost SPitch]
  , arLabels :: [ChordLabel]
  }
  deriving (Show)

class (Show algo) => ParseAlgo algo where
  runParse :: algo -> AlgoInput -> IO (Maybe AlgoResult)

type BeamWidth = Int
type UnspreadWidth = Int
type UnsplitWidth = Int
type ResevoirSize = Int
type MaxNotesPerSlice = Int

data AlgoType
  = RandomWalk
  | RandomWalkPerSegment
  | RandomSample
  | RandomReduction
  | BeamSearch BeamWidth
  | StochasticBeamSearch BeamWidth ResevoirSize
  | DualStochasticBeamSearch BeamWidth ResevoirSize
  | StochasticBeamSearchLimited BeamWidth ResevoirSize MaxNotesPerSlice
  | BeamSearchPerSegment BeamWidth
  | DualBeamSearch UnspreadWidth UnsplitWidth
  deriving (Show, Read, Eq)



instance ParseAlgo AlgoType where
  runParse algoType (AlgoInput eval inputSlices chords) = case algoType of
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
               pure $ Just $ AlgoResult slices (Just ops) chordGuesses

    RandomSample ->
      let x = splitSlicesIntoSegments eval sliceWrapper inputSlices
        in do
          path <- randomSamplePath (length chords)
          let slices = pathBetweens path
              chordGuesses = guessChords slices
           in pure $ Just (AlgoResult slices Nothing chordGuesses)

    RandomReduction ->
      let x = splitSlicesIntoSegments eval sliceWrapper inputSlices
       in Log.timedLog "Running Random Sample SBS Parse" $ do
        -- path <- randomSamplePathSBS x
        slices <- randomSamplePathSBS x

        -- let slices = pathBetweens path
        let chordGuesses = guessChords slices
         in pure $ Just $ AlgoResult slices Nothing chordGuesses

    StochasticBeamSearch beamWidth resevoirSize ->
      let initialState = SSFrozen $ pathFromSlices eval sliceWrapper inputSlices
       in
        Log.timedLog "Running Heuristic Search" $ do
          res <- runExceptT
            (stochasticBeamSearch
              beamWidth
              resevoirSize
              initialState
              (exploreStates sliceWrapper eval)
              (goalTest chords)
              (applyHeuristic heuristicZero)
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
               pure $ Just $ AlgoResult slices (Just ops) chordGuesses

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
              (applyHeuristic heuristicZero)
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
               pure $ Just $ AlgoResult slices (Just ops) chordGuesses

    StochasticBeamSearchLimited beamWidth resevoirSize maxNotesPerSlice ->
      let initialState = SSFrozen $ pathFromSlices eval sliceWrapper inputSlices
       in
        Log.timedLog "Running Heuristic Search" $ do
          res <- runExceptT
            (stochasticBeamSearchLimited
              beamWidth
              resevoirSize
              initialState
              (exploreStates sliceWrapper (protoVoiceEvaluatorLimitedSize' maxNotesPerSlice eval))
              (goalTest chords)
              (applyHeuristic heuristicZero)
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
               pure $ Just $ AlgoResult slices (Just ops) chordGuesses

    BeamSearch beamWidth ->
      let initialState = SSFrozen $ pathFromSlices eval sliceWrapper inputSlices
       in
        Log.timedLog "Running Heuristic Search" $ do
          res <- runExceptT
            (beamSearch
              beamWidth
              initialState
              (exploreStates sliceWrapper eval)
              (goalTest chords)
              (applyHeuristic heuristicZero)
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
               pure $ Just $ AlgoResult slices (Just ops) chordGuesses

protoVoiceEvaluatorLimitedSize'
  :: Int 
  -> Eval (Edges n) (t (Edge n)) (Notes n) (t2 n) (PVLeftmost n)
  -> Eval (Edges n) (t (Edge n)) (Notes n) (t2 n) (PVLeftmost n)
protoVoiceEvaluatorLimitedSize' n e = Eval filterUnspreadM vl vr mg t s
 where
  (Eval vm vl vr mg t s) = e

  filterUnspreadM (sl, tm, sr) = do 
    v <- vm (sl, tm, sr) 
    case v of 
      (Notes ns, v')
        |  MS.size ns < n -> Just (Notes ns, v')
        |  otherwise -> Nothing
