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

-- pathFromSlices
--   :: Eval (Edges SPC) [Edge SPC] (Notes SPC) [SPC] (PVLeftmost SPC)
--   -> [ ([(SPC, Bool)], Bool)]
--   -> Path (Maybe [Edge SPC], Bool) (Slice (Notes SPC))

-- | Entry point to the search algorithm
heuristicSearch'' 
  :: Show state
  => state -- starting state
  -> (state -> [state]) -- get adjacent states
  -> (state -> Bool) -- goal test
  -> (state -> Float) -- heuristic
  -> Maybe state -- output
heuristicSearch'' initialState getNextStates isGoalState heuristic = search $ heuristicSearchInit initialState heuristic isGoalState 
  where
    search hs
      | H.null (frontier hs) = Nothing
      | not (null goalStates) = trace "returning" Just (snd $ head goalStates)
      | otherwise = trace (  "\nCurrent head: " <> show (snd . head $ nearestState) 
                          <> "\nNew frontier: " <> show f'' ) 
          search $ hs {frontier = f''}
      where
        (nearestState, f') = H.splitAt 1 (frontier hs) -- pop lowest 0 from pq 
        -- Find neighboring states and costs
        nextStatesAndCosts = getNextStatesAndCosts (snd . head  $ nearestState) 
        -- Determine if any of these neighboring states are goal states
        goalStates = filter (\(c, s) -> isGoalState s) nextStatesAndCosts
        -- goalStates = H.toList $ H.filter (not . isGoalState) newStates

        f'' = trace ("Inserting " <> show nextStatesAndCosts <> " into " <> show f' ) 
          foldr H.insert f' nextStatesAndCosts

        getNextStatesAndCosts state = (\s -> (heuristic s, s)) <$> getNextStates state 

    --   expandFrontier :: H.MinPrioHeap Float state -> Either (H.MinPrioHeap Float state) state
    --   expandFrontier f = 
    --     case goalStates of 
    --       (s:sx) -> Right s 
    --       []     -> Left newFrontier 
    --     where 
    --       (h, f' ) = H.splitAt 0 f
    --       newStates = concatMap (getNextStates . snd) h
    --       goalStates = takeWhile isGoalState newStates
    --       newStates' = zip (heuristic <$> newStates) newStates -- refactor
    --       newFrontier = trace "jef" foldr H.insert f' newStates'

-- | Entry point to the search algorithm
heuristicSearch'
  :: forall state op
  .  state  -- Initial State 
  -> (state -> [state])  -- expand vertex
  -> (state -> Bool) -- goalState test
  -> (state -> Float) -- heuristic
  -> Maybe state -- output
heuristicSearch' initialState getNextStates isGoalState heuristic = searchLoop (H.singleton (0, initialState)) where
  searchLoop :: H.MinPrioHeap Float state -> Maybe state
  searchLoop frontier = undefined

    -- case expandFrontier frontier of
    --   Left newFrontier 
    --     | H.isEmpty newFrontier -> trace "No more steps" Nothing 
    --     | otherwise             -> trace ("more steps") searchLoop newFrontier
    --   Right goalState -> Just goalState
    -- where 
    --   expandFrontier :: H.MinPrioHeap Float state -> Either (H.MinPrioHeap Float state) state
    --   expandFrontier f = 
    --     case goalStates of 
    --       (s:sx) -> Right s 
    --       []     -> Left newFrontier 
    --     where 
    --       (h, f' ) = H.splitAt 0 f
    --       newStates = concatMap (getNextStates . snd) h
    --       goalStates = takeWhile isGoalState newStates
    --       newStates' = zip (heuristic <$> newStates) newStates -- refactor
    --       newFrontier = trace "jef" foldr H.insert f' newStates'
