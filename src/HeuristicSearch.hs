{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HeuristicSearch where

-- LOGGING
import qualified Data.Text as T
import Control.Logging qualified as Log

import Common
import Control.Monad.Except (ExceptT, lift, throwError)
import Data.Foldable
import qualified Data.Heap as H
import qualified Data.List as L
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

-- data HeuristicSearch s c = HeuristicSearch
--   { end :: Maybe s
--   , frontier :: eap c s
--   , heuristic :: (Maybe s, s) -> ExceptT String IO c
--   , goal :: s -> Bool
--   }

data HeuristicSearch s c = HeuristicSearch
  { frontier :: [(c, s)]
  , heuristic :: (Maybe s, s) -> ExceptT String IO c
  , goal :: s -> Bool
  }

heuristicSearchInit start heuristic' initCost goal' =
  HeuristicSearch
    { frontier = [(initCost, start)]
    , heuristic = heuristic'
    , goal = goal'
    }

-- heuristicSearchInit start heuristic' initCost goal' =
--   HeuristicSearch
--     { end = Nothing
--     , frontier = H.singleton (initCost, start)
--     , heuristic = heuristic'
--     , goal = goal'
--     }

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
    | null open = throwError "No Goal Found"
    | not $ null goalStates = pure . Log.traceL (T.pack (show . snd . head $ goalStates)) . snd . head $ goalStates
    | otherwise = do
        lift $ Log.log $ T.pack (show open )
        -- lift $ putStrLn "___________________________________________________"
        -- lift $ putStrLn "Frontier: "
        -- lift $ mapM_ print open
        -- lift $ putStrLn "Goals: "
        -- lift $ mapM_ print goalStates
        -- lift $ putStrLn "___________________________________________________"
        -- lift $ putStrLn "___________________________________________________"
        -- lift $ putStrLn "___________________________________________________
        -- Find neighboring states and costs
        nextStates <- foldlM getNextStatesAndCosts [] open

        -- nextStatesAndCosts <-
        --   let
        --     nextStateAndCost st = do
        --       h <- heuristic (Just nearestState, st)
        --       pure (h + cost, st)
        --    in
        --     mapM nextStateAndCost nextStates

        -- let nextStatesHeap = genHeap nextStatesAndCosts

        -- Determine if any of these neighboring states are goal states
        -- let goalStates = filter (isGoalState . snd) nextStates

        -- Add the new states to the frontier.
        -- Add lowest cost states
        -- Keeping a maximum of 5 states in the frontier at a time
        let newFrontier = take 4 nextStates
        --   H.fromList
        --   . H.take 1
        --   $ H.union nextStatesHeap remainingQueue

        search $
          hs{frontier = newFrontier}
   where
    open = frontier hs
    goalStates = filter (isGoalState . snd) open
    getNextStatesAndCosts xs (cost, state) = do
      nextStates <- getNextStates state
      statesAndCosts <- mapM go nextStates

      Log.timedLog (T.pack $ "Finding the minimum 12 of " ++ show (length nextStates)) $ pure $ minElems 12 [] statesAndCosts
      -- pure $ foldr (insertBy (comparing fst)) xs res
      -- pure $ foldr (:) xs res
     where
      go newState = do
        h <- heuristic (Just state, newState)
        pure (cost + h, newState)
      
      -- minElems :: Int -> Int -> (Ordering xs) -> [xs] -> [xs]  -> [xs]
      --
      minElems 0 mins [] = mins
      minElems 0 (m:mins) (x:rst)
        | fst x < fst m = minElems 0 (ins x mins) rst 
        | otherwise = minElems 0 (m:mins) rst
      minElems n mins (x:rst) = minElems (n-1) (ins x mins) rst
      minElems n mins [] = mins

      ins = L.insertBy ((flip . comparing) fst)
  -- | fst x < fst m = minElems 0 (L.insertBy ((flip . comparing) fst) x mins) rst
  -- | otherwise = minElems 0 mins rst

      

-- where
-- genNextStatesAndCosts

-- pure undefined

--   newStates <- getNextStates state
--   pure $ mapM (\newState -> pure (cost + heuristic (Just state, newState), newState)) newStates
-- getNextStateAndCost cost state = do
--   newCost <- heuristic

-- Pop the node in the frontier with the lowest priority
-- ((cost, nearestState), remainingQueue) = popFromHeap (frontier hs)

--
-- -- | Entry point to the search algorithm
-- heuristicSearch
--   :: Show state
--   => state -- starting state
--   -> (state -> ExceptT String IO [state]) -- get adjacent states
--   -> (state -> Bool) -- goal test
--   -> ((Maybe state, state) -> ExceptT String IO Double) -- heuristic
--   -> (state -> String) -- showAction
--   -> ExceptT String IO state -- output
-- heuristicSearch initialState getNextStates isGoalState heuristic printOp = do
--   initCost <- heuristic (Nothing, initialState)
--   search $ heuristicSearchInit initialState heuristic initCost isGoalState
--  where
--   search hs
--     | H.null (frontier hs) = throwError "No Goal Found"
--     | isGoalState nearestState = do pure nearestState
--     | otherwise = do
--         lift $ putStrLn "___________________________________________________"
--         lift $ putStrLn "Frontier: "
--         lift $ mapM_ print (frontier hs)
--
--         -- Find neighboring states and costs
--         nextStates <- getNextStates nearestState
--
--         nextStatesAndCosts <-
--           let
--             nextStateAndCost st = do
--               h <- heuristic (Just nearestState, st)
--               pure (h + cost, st)
--            in
--             mapM nextStateAndCost nextStates
--
--         let nextStatesHeap = genHeap nextStatesAndCosts
--
--         -- Determine if any of these neighboring states are goal states
--         let goalStates = filter (isGoalState . snd) nextStatesAndCosts
--
--         -- Add the new states to the frontier.
--         -- Add lowest cost states
--         -- Keeping a maximum of 5 states in the frontier at a time
--         let newFrontier = H.fromList . H.take 1 $ H.union nextStatesHeap remainingQueue
--
--         search $
--           hs{frontier = newFrontier}
--    where
--     -- Pop the node in the frontier with the lowest priority
--     ((cost, nearestState), remainingQueue) = popFromHeap (frontier hs)
--
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
