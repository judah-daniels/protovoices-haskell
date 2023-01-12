{-# LANGUAGE ScopedTypeVariables #-}

module HeuristicSearch where

import Common
import Control.Monad.Except (ExceptT, lift, throwError)
import Data.Foldable
import qualified Data.Heap as H
import Data.Maybe (fromMaybe)
import Data.Ord
import Debug.Trace
import HeuristicParser (getPathFromState)

data HeuristicSearch s c = HeuristicSearch
  { end :: Maybe s
  , frontier :: H.MinPrioHeap c s
  , heuristic :: s -> IO c
  , goal :: s -> Bool
  }

heuristicSearchInit start heuristic' initCost goal' =
  HeuristicSearch
    { end = Nothing
    , frontier = H.singleton (initCost, start)
    , heuristic = heuristic'
    , goal = goal'
    }

-- | Entry point to the search algorithm
heuristicSearch
  :: Show state
  => state -- starting state
  -> (state -> [state]) -- get adjacent states
  -> (state -> Bool) -- goal test
  -> (state -> IO Float) -- heuristic
  -> (state -> String) -- showAction
  -> ExceptT String IO state -- output
heuristicSearch initialState getNextStates isGoalState heuristic printOp = do
  initCost <- lift $ heuristic initialState
  search $ heuristicSearchInit initialState heuristic initCost isGoalState
 where
  search hs
    | H.null (frontier hs) = throwError "No Goal Found"
    | isGoalState nearestState = do pure nearestState
    | otherwise = do
        lift $ mapM_ print (frontier hs)

        -- Find neighboring states and costs
        -- Build as heap
        -- let nextStatesHeap = H.empty
        nextStatesAndCosts <-
          let
            nextStateAndCost st = do
              h <- heuristic st
              pure (cost + h, st)
           in
            lift $ mapM nextStateAndCost (getNextStates nearestState)

        let nextStatesHeap = genHeap nextStatesAndCosts

        -- Determine if any of these neighboring states are goal states
        let goalStates = filter (isGoalState . snd) nextStatesAndCosts

        -- Add the new states to the frontier.
        -- Add lowest cost states
        -- Keeping a maximum of 5 states in the frontier at a time
        -- let newFrontier = foldr (insertLimitedBy 30) remainingQueue nextStatesAndCosts
        let newFrontier = H.fromList . H.take 5 $ H.union nextStatesHeap remainingQueue

        search $
          hs{frontier = newFrontier}
   where
    -- Pop the node in the frontier with the lowest priority
    ((cost, nearestState), remainingQueue) = popFromHeap (frontier hs)

getLowestCostState :: [(Float, state)] -> state
getLowestCostState goalStates = snd $ maximumBy (comparing fst) goalStates

popFromHeap :: H.HeapItem a b => H.Heap a b -> (b, H.Heap a b)
popFromHeap heap =
  let (item : _, remaining) = H.splitAt 1 heap
   in (item, remaining)

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
