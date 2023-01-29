{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.Aeson.KeyMap (singleton)
import System.Random (initStdGen)
import System.Random.Stateful
  ( StatefulGen
  , newIOGenM
  , uniformRM
  )

data HeuristicSearch s c = HeuristicSearch
  { end :: Maybe s
  , frontier :: H.MinPrioHeap c s
  , heuristic :: (Maybe s, s) -> ExceptT String IO c
  , goal :: s -> Bool
  }

data RandomSearch s = RandomSearch
  { rsEnd :: Maybe s
  , rsHead :: s
  , rsGoal :: s -> Bool
  }

randomSearchInit start goal =
  RandomSearch
    { rsEnd = Nothing
    , rsHead = start
    , rsGoal = goal
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
  -> (state -> ExceptT String IO [state]) -- get adjacent states
  -> (state -> Bool) -- goal test
  -> ((Maybe state, state) -> ExceptT String IO Double) -- heuristic
  -> (state -> String) -- showAction
  -> ExceptT String IO state -- output
heuristicSearch initialState getNextStates isGoalState heuristic printOp = do
  initCost <- heuristic (Nothing, initialState)
  search $ heuristicSearchInit initialState heuristic initCost isGoalState
 where
  search hs
    | H.null (frontier hs) = throwError "No Goal Found"
    | isGoalState nearestState = do pure nearestState
    | otherwise = do
        lift $ putStrLn "___________________________________________________"
        lift $ putStrLn "Frontier: "
        lift $ mapM_ print (frontier hs)

        -- Find neighboring states and costs
        nextStates <- getNextStates nearestState

        nextStatesAndCosts <-
          let
            nextStateAndCost st = do
              h <- heuristic (Just nearestState, st)
              pure (h + cost, st)
           in
            mapM nextStateAndCost nextStates

        let nextStatesHeap = genHeap nextStatesAndCosts

        -- Determine if any of these neighboring states are goal states
        let goalStates = filter (isGoalState . snd) nextStatesAndCosts

        -- Add the new states to the frontier.
        -- Add lowest cost states
        -- Keeping a maximum of 5 states in the frontier at a time
        let newFrontier = H.fromList . H.take 1 $ H.union nextStatesHeap remainingQueue

        search $
          hs{frontier = newFrontier}
   where
    -- Pop the node in the frontier with the lowest priority
    ((cost, nearestState), remainingQueue) = popFromHeap (frontier hs)

getLowestCostState :: [(Double, state)] -> state
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

{- | Entry point to the search algorithm
Picks random choice from a split or spread operation
-}
randomSampleFromSegmentSearch
  -- Just take a input slices.

  :: Show state
  => state -- starting state
  -> (state -> ExceptT String IO [state]) -- get adjacent states
  -> (state -> Bool) -- goal test
  -> (state -> String) -- showAction
  -> ExceptT String IO state -- output
randomSampleFromSegmentSearch initialState getNextStates isGoalState printOp = do
  gen <- lift initStdGen
  mgen <- lift $ newIOGenM gen
  search mgen $ randomSearchInit initialState isGoalState
 where
  search g hs
    -- \| (hs ) = throwError "No Goal Found"
    | isGoalState nearestState = do pure nearestState
    | otherwise = do
        lift $ putStrLn "___________________________________________________"
        lift $ putStrLn "Head: "
        lift $ print (rsHead hs)

        -- Find neighboring states and costs
        nextStates <- getNextStates nearestState
        lift $ putStrLn $ "Considering " <> show (length nextStates) <> " states"

        case nextStates of
          [] -> throwError "Parse Stuck! Perhaps the number of chords and segments are not the same?"
          xs -> do
            newHead <- do
              res <- pickRandom g nextStates
              case res of
                Nothing -> throwError "No Goal found"
                Just s -> pure s
            search g $
              hs{rsHead = newHead}
   where
    -- Pop the node in the frontier with the lowest priority
    nearestState = rsHead hs

{- | Entry point to the search algorithm
Picks random choice from a split or spread operation
-}
randomChoiceSearch
  :: Show state
  => state -- starting state
  -> (state -> ExceptT String IO [state]) -- get adjacent states
  -> (state -> Bool) -- goal test
  -> (state -> String) -- showAction
  -> ExceptT String IO state -- output
randomChoiceSearch initialState getNextStates isGoalState printOp = do
  gen <- lift initStdGen
  mgen <- lift $ newIOGenM gen
  search mgen $ randomSearchInit initialState isGoalState
 where
  search g hs
    -- \| (hs ) = throwError "No Goal Found"
    | isGoalState nearestState = do pure nearestState
    | otherwise = do
        lift $ putStrLn "___________________________________________________"
        lift $ putStrLn "Head: "
        lift $ print (rsHead hs)

        -- Find neighboring states and costs
        nextStates <- getNextStates nearestState
        lift $ putStrLn $ "Considering " <> show (length nextStates) <> " states"

        case nextStates of
          [] -> throwError "Parse Stuck! Perhaps the number of chords and segments are not the same?"
          xs -> do
            newHead <- do
              res <- pickRandom g nextStates
              case res of
                Nothing -> throwError "No Goal found"
                Just s -> pure s
            search g $
              hs{rsHead = newHead}
   where
    -- Pop the node in the frontier with the lowest priority
    nearestState = rsHead hs

-- ((cost, nearestState), remainingQueue) = popFromHeap (frontier hs)

pickRandom :: StatefulGen g m => g -> [slc] -> m (Maybe slc)
pickRandom _ [] = pure Nothing
pickRandom gen xs = do
  i <- uniformRM (0, length xs - 1) gen
  pure $ Just $ xs !! i
