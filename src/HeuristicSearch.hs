{-# LANGUAGE ScopedTypeVariables #-}

module HeuristicSearch where

import Debug.Trace
import Common
import Control.Applicative
import HeuristicParser
import PVGrammar
import PVGrammar.Parse
import qualified Data.Heap as H
import Data.List

data HeuristicSearch s c = HeuristicSearch 
  { end :: Maybe s
  , frontier :: H.MinPrioHeap c s
  , heuristic :: s -> c 
  , goal :: s -> Bool 
  }

heuristicSearchInit start heuristic' goal' = HeuristicSearch 
  { end = Nothing
  , frontier = H.singleton (heuristic' start, start)
  , heuristic = heuristic'
  , goal = goal'
  }

-- | Entry point to the search algorithm
heuristicSearch'' 
  :: Show state
  => state              -- starting state
  -> (state -> [state]) -- get adjacent states
  -> (state -> Bool)    -- goal test
  -> (state -> Float)   -- heuristic
  -> Maybe state        -- output
heuristicSearch'' initialState getNextStates isGoalState heuristic = search $ heuristicSearchInit initialState heuristic isGoalState 
  where
    search hs
      | H.null (frontier hs) = Nothing
      | not (null goalStates) = trace "returning" Just (snd $ head goalStates)
      | otherwise = trace (  "\nCurrent head: " <> show (snd . head $ nearestState) 
                          <> "\nNew frontier: " <> show newFrontier ) 
          search $ hs {frontier = newFrontier}
      where
        -- Pop the node in the frontier with the lowest priority
        (nearestState, remainingQueue) = H.splitAt 1 (frontier hs) -- pop lowest 0 from pq 

        -- Find neighboring states and costs
        nextStatesAndCosts = getNextStatesAndCosts (snd . head  $ nearestState) 

        -- Determine if any of these neighboring states are goal states
        goalStates = filter (isGoalState . snd) nextStatesAndCosts

        -- Add the new states to the frontier.
        newFrontier = trace ("Inserting " <> show nextStatesAndCosts <> " into " <> show remainingQueue ) 
          foldr H.insert remainingQueue nextStatesAndCosts
     
        getNextStatesAndCosts state = (\s -> (heuristic s, s)) <$> getNextStates state 
