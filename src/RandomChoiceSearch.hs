{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RandomChoiceSearch where

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

data RandomSearch s = RandomSearch
  { rsHead :: s
  , rsGoal :: s -> Bool
  }

randomSearchInit start goal =
  RandomSearch
    { rsHead = start
    , rsGoal = goal
    }

{- | Entry point to the search algorithm
Picks random choice from a split or spread operation
Skewed towards splits.
-}
randomChoiceSearch
  -- Just take a input slices.
  :: Show state
  => state -- starting state
  -> (state -> ExceptT String IO [state]) -- get adjacent states
  -> (state -> Bool) -- goal test
  -> (state -> String) -- showAction
  -> ExceptT String IO state -- output
randomChoiceSearch initialState getNextStates isGoalState printOp = do
  gen <- lift initStdGen
  mgen <- lift $ newIOGenM gen
  search 0 mgen $ randomSearchInit initialState isGoalState
 where
  search maxN g hs
    | isGoalState nearestState = do
        lift $ putStrLn $ "Max number of options reached " <> show maxN
        pure nearestState
    | otherwise = do
        lift $ putStrLn "___________________________________________________"
        lift $ putStrLn "Head: "
        lift $ print nearestState

        -- Find neighboring states
        nextStates <- getNextStates nearestState
        let numStates = length nextStates

        lift $ putStrLn $ "Choosing randomly from " <> show numStates <> " states"

        case nextStates of
          [] -> throwError "Parse Stuck! Perhaps the number of chords and segments are not the same?"
          xs -> do
            newHead <- do
              res <- pickRandom g nextStates
              case res of
                Nothing -> throwError "No Goal found"
                Just s -> pure s
            search (max maxN numStates) g $
              hs{rsHead = newHead}
   where
    nearestState = rsHead hs

randomChoiceSearchSingleSegment
  -- Just take a input slices.
  :: Show state
  => state -- starting state
  -> (state -> ExceptT String IO [state]) -- get adjacent states
  -> (state -> Bool) -- goal test
  -> (state -> String) -- showAction
  -> ExceptT String IO state -- output
randomChoiceSearchSingleSegment initialState getNextStates isGoalState printOp = do
  gen <- lift initStdGen
  mgen <- lift $ newIOGenM gen
  search 0 mgen $ randomSearchInit initialState isGoalState
 where
  search maxN g hs
    | isGoalState nearestState = do
        -- lift $ putStrLn $ "Max number of options reached " <> show maxN
        pure nearestState
    | otherwise = do
        -- lift $ putStrLn "___________________________________________________"
        -- lift $ putStrLn "Head: "
        lift $ print nearestState

        -- Find neighboring states
        nextStates <- getNextStates nearestState
        let numStates = length nextStates

        -- lift $ putStrLn $ "Choosing randomly from " <> show numStates <> " states"

        case nextStates of
          [] -> throwError "Parse Stuck! Perhaps the number of chords and segments are not the same?"
          xs -> do
            newHead <- do
              res <- pickRandom g nextStates
              case res of
                Nothing -> throwError "No Goal found"
                Just s -> pure s
            search (max maxN numStates) g $
              hs{rsHead = newHead}
   where
    nearestState = rsHead hs
pickRandom :: StatefulGen g m => g -> [slc] -> m (Maybe slc)
pickRandom _ [] = pure Nothing
pickRandom gen xs = do
  i <- uniformRM (0, length xs - 1) gen
  pure $ Just $ xs !! i
