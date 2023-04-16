{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm.HeuristicSearch
  (
    heuristicSearch
  , beamSearch
  )
    where

import Control.Logging qualified as Log
import Data.Text qualified as T
import Data.Foldable ( foldlM, maximumBy )
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Ord ( comparing )

import Musicology.Core ( SPitch )
import PVGrammar ( Edge, Edges, Freeze, Notes, Split, Spread )
import Common
    ( Leftmost(..),
      LeftmostDouble(..),
      LeftmostSingle(..)
    )

import Control.Monad.Except (ExceptT, lift, throwError)

import HeuristicParser (SearchState, getOpFromState)
import Data.List (sortBy)
import Data.Function


data HeuristicSearch s c = HeuristicSearch
  { frontier :: [(c, s)]
  , heuristic :: (Maybe s, s) -> ExceptT String IO c
  , goal :: s -> Bool
  }

type SearchState' =
  SearchState
    (Edges SPitch)
    [Edge SPitch]
    (Notes SPitch)
    (Leftmost (Split SPitch) Freeze (Spread SPitch))

heuristicSearchInit start heuristic' initCost goal' =
  HeuristicSearch
    { frontier = [(initCost, start)]
    , heuristic = heuristic'
    , goal = goal'
    }

-- | Entry point to the search algorithm
beamSearch
  :: Int -- Hyperparameters
  -> SearchState' -- starting state
  -> (SearchState' -> ExceptT String IO [SearchState']) -- get adjacent states
  -> (SearchState' -> Bool) -- goal test
  -> ((Maybe SearchState', SearchState') -> ExceptT String IO Double) -- heuristic
  -> ExceptT String IO SearchState' -- output
beamSearch beamWidth initialState getNextStates isGoalState heuristic = do
  initCost <- heuristic (Nothing, initialState)
  search $ heuristicSearchInit initialState heuristic initCost isGoalState
 where
  search hs
    | null open = throwError "No Goal Found"
    | not $ null goalStates = pure . snd . head $ goalStates
    | otherwise = do
        lift $ Log.log $ T.pack (show open)

        nextNodesAndCosts <- mapM getNextStatesAndCosts open
        let nextStates = minElems beamWidth [] (concat nextNodesAndCosts)
        search $
          hs{frontier = nextStates}
   where
    -- getNextNodes nodes = do

      -- allNextNodes <- concatMapM (\(_, n) -> zip (repeat n) <$> getNextStatesAndCosts n) nodes
      -- sortedNextNodes <- mapM (\(cost, n) -> (\h -> (cost + h, n)) <$> heuristic n) allNextNodes
      -- return $ take beamWidth $ sortBy (\(cost1, _) (cost2, _) -> compare cost1 cost2) sortedNextNodes

    -- getNextStates node = replicateM 3 (return $ node + 1)

    open = frontier hs
    goalStates = filter (isGoalState . snd) open

    minElems 0 mins [] = mins
    minElems 0 (m : mins) (x : rst)
      | fst x < fst m = minElems 0 (ins x mins) rst
      | otherwise = minElems 0 (m : mins) rst
    minElems n mins (x : rst) = minElems (n - 1) (ins x mins) rst
    minElems n mins [] = mins

    ins = L.insertBy ((flip . comparing) fst)

    getNextStatesAndCosts (cost, state) = do
      nextStates <- getNextStates state
      mapM go nextStates
     where
      go newState = do
        h <- heuristic (Just state, newState)
        pure (cost + h, newState)

-- | Entry point to the search algorithm
heuristicSearch
  :: Int -- Hyperparameters
  -> SearchState' -- starting state
  -> (SearchState' -> ExceptT String IO [SearchState']) -- get adjacent states
  -> (SearchState' -> Bool) -- goal test
  -> ((Maybe SearchState', SearchState') -> ExceptT String IO Double) -- heuristic
  -> ExceptT String IO SearchState' -- output
heuristicSearch beamWidth initialState getNextStates isGoalState heuristic = do
  initCost <- heuristic (Nothing, initialState)
  search $ heuristicSearchInit initialState heuristic initCost isGoalState
 where
  search hs
    | null open = throwError "No Goal Found"
    | not $ null goalStates = pure . snd . head $ goalStates
    | otherwise = do
        lift $ Log.log $ T.pack (show open)

        nextStates <- foldlM getNextStatesAndCosts [] open
        let nextUnfreezeStates = minElems 1 [] $ filter (isFreeze . snd) nextStates
            nextUnspreadStates = minElems beamWidth [] $ filter (isSpread . snd) nextStates
            nextUnsplitStates = minElems beamWidth [] $ filter (isSplit . snd) nextStates
            newFrontier = nextUnfreezeStates <> nextUnsplitStates
        search $
          hs{frontier = newFrontier}
   where
    open = frontier hs
    goalStates = filter (isGoalState . snd) open

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
