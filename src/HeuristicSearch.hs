{-# LANGUAGE ScopedTypeVariables #-}

module HeuristicSearch where

import qualified Data.Heap as H
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
    | not (null goalStates) = trace ("Goal State(s) found: " <> show (snd <$> goalStates)) Just (snd $ head goalStates)
    | otherwise = trace ("Current State: \n" <> show (snd . head $ nearestState) <> "\n") search $ hs{frontier = newFrontier}
   where
    -- Pop the node in the frontier with the lowest priority
    (nearestState, remainingQueue) = H.splitAt 1 (frontier hs) -- pop lowest 0 from pq

    -- Find neighboring states and costs
    nextStatesAndCosts = getNextStatesAndCosts (snd . head $ nearestState)

    -- Determine if any of these neighboring states are goal states
    goalStates = filter (isGoalState . snd) nextStatesAndCosts

    -- Add the new states to the frontier.
    -- newFrontier = trace ("Inserting " <> show nextStatesAndCosts <> " into " <> show remainingQueue )
    newFrontier = foldr H.insert remainingQueue nextStatesAndCosts

    getNextStatesAndCosts state = (\s -> (heuristic s, s)) <$> getNextStates state
