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
  )
  where

import Control.Logging qualified as Log
import Data.Text qualified as T
import FileHandling ( InputSlice (..), pathFromSlices, splitSlicesIntoSegments)
import PBHModel 
import Algorithm.RandomChoiceSearch
import Algorithm.RandomSampleParser
import Algorithm.HeuristicSearch

import Common ( Leftmost(..) , LeftmostDouble(..) , LeftmostSingle(..), Eval, pathBetweens)

import Control.Monad.Except (ExceptT, lift, throwError)
import Data.Maybe (fromMaybe, fromJust)

import HeuristicParser (sliceWrapper, idWrapper, SearchState (..), wrapSlice, sLbl, SearchState, getOpFromState, getPathFromState, showOp, goalTest, SliceWrapper, guessChords, exploreStates, getOpsFromState, SliceWrapped (sWContent))
import Musicology.Core ( SPitch, showNotation )
import PVGrammar ( Edge, Edges, Freeze, Notes, Split, Spread, PVLeftmost )
import Control.Monad.Trans.Except (runExceptT)
import Heuristics

data AlgoInput = 
  AlgoInput 
  (Eval (Edges SPitch) [Edge SPitch] (Notes SPitch) [SPitch] (PVLeftmost SPitch))
  HarmonicProfileData
  [InputSlice SPitch]
  [ChordLabel]

data AlgoResult = AlgoResult 
  { arTop :: [Notes SPitch]
  , arOps :: Maybe [PVLeftmost SPitch]
  , arLabels :: [ChordLabel] 
  }
  deriving (Show)

class (Show a, Eq a) => ParseAlgo a where 
  runParse :: a -> AlgoInput -> IO (Maybe AlgoResult)

data AlgoType
  = RandomParse
  | RandomParseSBS
  | RandomSample
  | RandomSampleSBS
  | Heuristic1
  | HeuristicSBS1
  | All
  deriving (Read, Show, Eq)

timeOutMs = 400 * 1000000 :: Int

instance ParseAlgo AlgoType where
  runParse algoType (AlgoInput eval params inputSlices chords) = case algoType of   
    RandomParse -> 
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
                  chordGuesses = guessChords params slices 
               in 
               pure $ Just $ AlgoResult slices (Just ops) chordGuesses

    RandomSample -> 
      let x = splitSlicesIntoSegments eval (sliceWrapper params) inputSlices
        in do 
          path <- randomSamplePath (length chords)
          let slices = pathBetweens path
              chordGuesses = guessChords params slices
           in pure $ Just (AlgoResult slices Nothing chordGuesses)

    RandomSampleSBS -> 
      let x = splitSlicesIntoSegments eval (sliceWrapper params) inputSlices
       in Log.timedLog "Running Random Sample SBS Parse" $ do
        path <- randomSamplePathSBS x

        let slices = pathBetweens path
            chordGuesses = guessChords params slices
         in pure $ Just $ AlgoResult slices Nothing chordGuesses 

    Heuristic1 -> 
      let initialState = SSFrozen $ pathFromSlices eval (sliceWrapper params) inputSlices
       in 
        Log.timedLog "Running Random Sample SBS Parse" $ do
          res <- runExceptT 
            (heuristicSearch 
              initialState 
              (exploreStates (sliceWrapper params) eval) 
              (goalTest chords) 
              (applyHeuristic 
              (testHeuristic params)
            ) 
            (showOp . getOpsFromState))

          case res of
            Left err -> do
              Log.warn $ T.pack err
              pure Nothing
            Right finalState -> 
              let p = fromJust $ getPathFromState finalState
                  ops = getOpsFromState finalState
                  slices = pathBetweens p
                  chordGuesses = guessChords params slices
               in 
               pure $ Just $ AlgoResult slices (Just ops) chordGuesses

