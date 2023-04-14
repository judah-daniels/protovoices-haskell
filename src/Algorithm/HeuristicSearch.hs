{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm.HeuristicSearch 
  (
    heuristicSearch
  )where

-- LOGGING

import Control.Logging qualified as Log
import Data.Text qualified as T

import Common
    ( Leftmost(..),
      LeftmostDouble(..),
      LeftmostSingle(..) 
    )

import Control.Monad.Except (ExceptT, lift, throwError)

import Data.Foldable ( foldlM, maximumBy )

import Data.Heap qualified as H
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Ord ( comparing )
import HeuristicParser (sliceWrapper, wrapSlice, sLbl, SearchState, getOpFromState, getPathFromState)

import Data.Aeson.KeyMap (singleton)
import System.Random (initStdGen)
import System.Random.Stateful
  ( StatefulGen
  , newIOGenM
  , uniformRM
  )

import Musicology.Core ( SPitch )
import Musicology.Core qualified as Music
import PVGrammar ( Edge, Edges, Freeze, Notes, Split, Spread )

data HeuristicSearch s c = HeuristicSearch
  { frontier :: [(c, s)]
  , heuristic :: (Maybe s, s) -> ExceptT String IO c
  , goal :: s -> Bool
  }

type SearchState' = SearchState (Edges SPitch) [Edge SPitch] (Notes SPitch) (Leftmost (Split SPitch) Freeze (Spread SPitch))

heuristicSearchInit start heuristic' initCost goal' =
  HeuristicSearch
    { frontier = [(initCost, start)]
    , heuristic = heuristic'
    , goal = goal'
    }

-- | Entry point to the search algorithm
heuristicSearch
  :: SearchState' -- starting state
  -> (SearchState' -> ExceptT String IO [SearchState']) -- get adjacent states
  -> (SearchState' -> Bool) -- goal test
  -> ((Maybe SearchState', SearchState') -> ExceptT String IO Double) -- heuristic
  -> (SearchState' -> String) -- showAction
  -> ExceptT String IO SearchState' -- output
heuristicSearch initialState getNextStates isGoalState heuristic printOp = do
  initCost <- heuristic (Nothing, initialState)
  search $ heuristicSearchInit initialState heuristic initCost isGoalState
 where
  search hs
    | null open = throwError "No Goal Found"
    | not $ null goalStates = pure . snd . head $ goalStates
    | otherwise = do
        lift $ Log.log $ T.pack (show open)

        -- nextStates <- foldlM getNextStatesAndCosts [] open
        nextStates <- foldlM getNextStatesAndCosts [] open
        let nextUnfreezeStates = minElems 1 [] $ filter (isFreeze . snd) nextStates
            nextUnspreadStates = minElems 5 [] $ filter (isSpread . snd) nextStates
            nextUnsplitStates = minElems 10 [] $ filter (isSplit . snd) nextStates

        let newFrontier = nextUnfreezeStates <> nextUnsplitStates <> nextUnspreadStates

        search $
          hs{frontier = newFrontier}
   where
    open = frontier hs
    goalStates = filter (isGoalState . snd) open

    isFreeze :: SearchState' -> Bool
    isFreeze state = case getOpFromState state of
      Nothing -> False
      Just op -> opType op == Freeze'

    isSplit :: SearchState' -> Bool
    isSplit state = case getOpFromState state of
      Nothing -> False
      Just op -> opType op == Split'

    isSpread :: SearchState' -> Bool
    isSpread state = case getOpFromState state of
      Nothing -> False
      Just op -> opType op == Spread'

    minElems 0 mins [] = mins
    minElems 0 (m : mins) (x : rst)
      | fst x < fst m = minElems 0 (ins x mins) rst
      | otherwise = minElems 0 (m : mins) rst
    minElems n mins (x : rst) = minElems (n - 1) (ins x mins) rst
    minElems n mins [] = mins

    ins = L.insertBy ((flip . comparing) fst)

    getNextStatesAndCosts xs (cost, state) = do
      nextStates <- getNextStates state
      mapM go nextStates
     where
      go newState = do
        h <- heuristic (Just state, newState)
        pure (cost + h, newState)

getLowestCostState :: [(Double, state)] -> state
getLowestCostState goalStates = snd $ maximumBy (comparing fst) goalStates

popFromHeap :: H.HeapItem a b => H.Heap a b -> (b, H.Heap a b)
popFromHeap heap =
  let (item : _, remaining) = H.splitAt 1 heap
   in (item, remaining)

-- Split
data PvOp = Split' | Spread' | Freeze' deriving (Eq)

opType :: Leftmost a b c -> PvOp
opType op = case op of
  LMSingle lms ->
    case lms of
      LMSingleSplit _ -> Split'
      LMSingleFreeze _ -> Freeze'
  LMDouble lmd ->
    case lmd of
      LMDoubleFreezeLeft _ -> Freeze'
      LMDoubleSpread _ -> Spread'
      LMDoubleSplitLeft _ -> Split'
      LMDoubleSplitRight _ -> Split'

genHeap
  :: (Ord a)
  => [(a, b)]
  -> H.MinPrioHeap a b
genHeap = foldr H.insert H.empty

insertHeapLimitedBy n item heap
  | H.size heap >= n = let heap' = (fromMaybe undefined $ H.viewTail heap) in H.insert item heap'
  | otherwise = H.insert item heap

-- insertLimited :: H.HeapItem pol state => Int -> item -> H.Heap pol item -> H.Heap pol item
insertLimitedBy n item heap
  | H.size heap >= n = let heap' = (fromMaybe undefined $ H.viewTail heap) in H.insert item heap'
  | otherwise = H.insert item heap
