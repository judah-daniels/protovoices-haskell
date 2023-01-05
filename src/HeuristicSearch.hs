{-# LANGUAGE ScopedTypeVariables #-}

module HeuristicSearch where

import qualified Data.Heap as H
import Data.Maybe (fromMaybe)
import Debug.Trace
import HeuristicParser (getPathFromState)

data HeuristicSearch s c = HeuristicSearch
  { end :: Maybe s
  , frontier :: H.MinPrioHeap c s
  , heuristic :: s -> c
  , goal :: s -> Bool
  }

heuristicSearchInit start heuristic' goal' =
  HeuristicSearch
    { end = Nothing
    , frontier = H.singleton (heuristic' start, start)
    , heuristic = heuristic'
    , goal = goal'
    }

-- | Entry point to the search algorithm
heuristicSearch
  :: Show state
  => state -- starting state
  -> (state -> [state]) -- get adjacent states
  -> (state -> Bool) -- goal test
  -> (state -> Float) -- heuristic
  -> (state -> String)
  -> Maybe state -- output
heuristicSearch initialState getNextStates isGoalState heuristic printOp = trace "Starting Heuristic Search " search $ heuristicSearchInit initialState heuristic isGoalState
 where
  search hs
    | H.null (frontier hs) = trace "Dead end, no goal found." Nothing
    -- \| not (null goalStates) = trace "returning" Just (snd $ head goalStates)
    -- \| otherwise = trace (  "\nCurrent head: " <> show (snd . head $ nearestState)
    --                     <> "\nNew frontier: " <> show newFrontier )
    | not (null goalStates) =
        trace
          ("\n Open: " <> show (H.toList (frontier hs)))
          -- ( "Exploring: \n"
          --     <> show (snd . head $ nearestState)
          --     <> "\n    "
          --     <> printOp (snd . head $ nearestState)
          --     <> "\n\n\n"
          --     <> "Goal State(s) found: "
          --     <> show (snd <$> goalStates)
          --     <> "\n    "
          --     <> printOp (snd . head $ goalStates)
          --     <> "\n"
          -- )
          Just
          (snd . head $ goalStates)
    | otherwise =
        trace
          -- ( "Exploring: \n"
          --     <> show (snd . head $ nearestState)
          --     <> "\n    "
          --     <> printOp (snd . head $ nearestState)
          --     <> "\n"
          -- )
          ("\nFrontier: " <> show (snd <$> H.toList (frontier hs)))
          search
          $ hs{frontier = newFrontier}
   where
    -- Pop the node in the frontier with the lowest priority
    (nearestState, remainingQueue) = H.splitAt 1 (frontier hs) -- pop lowest 0 from pq
    getNextStatesAndCosts state = (\s -> (heuristic s, s)) <$> getNextStates state

    -- Find neighboring states and costs
    nextStatesAndCosts = getNextStatesAndCosts (snd . head $ nearestState)

    -- Determine if any of these neighboring states are goal states
    goalStates = filter (isGoalState . snd) nextStatesAndCosts

    -- Add the new states to the frontier.
    -- Keeping 5 states in the front at a time
    -- newFrontier = trace ("Inserting " <> show nextStatesAndCosts <> " into " <> show remainingQueue )
    newFrontier = foldr (insertLimitedBy 1) remainingQueue nextStatesAndCosts
    -- newFrontier = foldr H.insert remainingQueue nextStatesAndCosts

    -- insertLimited :: H.HeapItem pol state => Int -> item -> H.Heap pol item -> H.Heap pol item
    insertLimitedBy n item heap
      | H.size heap >= n = let heap' = (fromMaybe undefined $ H.viewTail heap) in H.insert item heap'
      | otherwise = H.insert item heap
