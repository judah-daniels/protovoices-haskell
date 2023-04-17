{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Algorithm.HeuristicSearch
  (
    -- heuristicSearch
  aab
    ,stochasticBeamSearch
    ,stochasticBeamSearchLimited
    , beamSearch
    ,reservoirSample
  )
    where

import Control.Logging qualified as Log
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Foldable ( foldlM, maximumBy )
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Ord ( comparing )

import Musicology.Core ( SPitch )

import PVGrammar

import Common
    ( Leftmost(..),
      LeftmostDouble(..),
      LeftmostSingle(..), Eval (..)
    )

import Control.Monad.Except (ExceptT, lift, throwError)

import HeuristicParser (SearchState, getOpFromState)
import Data.List (sortBy)
import Data.Function
import System.Random.Stateful (newIOGenM, StatefulGen, uniformRM)
import System.Random
import qualified Internal.MultiSet as MS


data HeuristicSearch s c = HeuristicSearch
  { frontier :: [(c, s)]
  , heuristic :: (s, s) -> ExceptT String IO c
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

aab :: Int -> [a] -> IO [a]
aab k xs = do
  gen <- initStdGen
  mgen <- newIOGenM gen
  reservoirSample mgen k xs

-- With a simple list, this is pretty instant as long as the total list is less than 1000000
-- Cost is based on size of list rather than reservoir size. 
reservoirSample :: StatefulGen g IO => g -> Int -> [a] -> IO [a]
reservoirSample gen k xs
  | n <= k = pure xs
  | otherwise = do
     let res = V.fromList $ take k xs
     r <- uniformRM (0::Double,1::Double) gen
     let w = exp (log r / fromIntegral (k-1))
     z <- go gen k res w (drop k xs)
     pure $ V.toList z
       where
         -- go :: g -> Int -> V.Vector t -> Double -> [t] -> V.Vector t 
         go gen i res w [] = pure res
         go gen i res w xs = do
           r <- uniformRM (0, 1) gen
           let jump = floor (logBase (1-w) r) + 1
           if i+jump > n-1
              then pure res
              else case drop jump xs of
                [] -> pure res
                xss@(x:_) -> do
                   let xss = drop jump xs
                   rint <- uniformRM (0, k-1) gen
                   r <- uniformRM (0,1) gen
                   let newres = res V.// [(rint, x)]
                   let w' = w * exp (log r / fromIntegral k)
                   go gen (i+jump) newres w' xss
         n = length xs

stochasticBeamSearchLimited
  :: Int -- Hyperparameters
  -> Int
  -> SearchState' -- starting state
  -> (SearchState' -> ExceptT String IO [SearchState']) -- get adjacent states
  -> (SearchState' -> Bool) -- goal test
  -> ((SearchState', SearchState') -> ExceptT String IO Double) -- heuristic
  -> ExceptT String IO SearchState' -- output
stochasticBeamSearchLimited beamWidth resevoir initialState getNextStates isGoalState heuristic = do
  gen <- lift initStdGen
  mgen <- lift $ newIOGenM gen
  search mgen $ heuristicSearchInit initialState heuristic 0 isGoalState
 where
  search mgen hs
    | null open = throwError "No Goal Found"
    | null open = throwError "No Goal Found"
    | not $ null goalStates = pure . snd . head $ goalStates
    | otherwise = do
        nextStatesAll <- mapM
          (\(oldcost, s) -> do
            n <- getNextStates s
            pure $ map ((oldcost,s),) n
          ) open
        let allNextStates = concat nextStatesAll
        let l = length allNextStates
        -- Take a sample from list instead of all of them 
        -- truncated <- pure $ take resevoir allNextStates
        truncated <- lift $ reservoirSample mgen resevoir allNextStates
        nextWithCosts <- mapM doHeuristic truncated

        let nextStates = minElems beamWidth [] nextWithCosts
        search mgen $
          hs{frontier = nextStates}
   where
    getNextStatesWithPrev state = do
      next <- getNextStates state
      pure $ (state,) <$> next

    open = frontier hs
    goalStates = filter (isGoalState . snd) open

    minElems 0 mins [] = mins
    minElems 0 (m : mins) (x : rst)
      | fst x < fst m = minElems 0 (ins x mins) rst
      | otherwise = minElems 0 (m : mins) rst
    minElems n mins (x : rst) = minElems (n - 1) (ins x mins) rst
    minElems n mins [] = mins

    ins = L.insertBy ((flip . comparing) fst)

    doHeuristic ((oldcost, prev), curr) = do
      cost <- heuristic (prev,curr)
      pure (oldcost + cost, curr)

    getNextStatesAndCosts (cost, state) = do
      nextStates <- getNextStates state
      mapM go nextStates
     where
      go newState = do
        h <- heuristic (state, newState)
        pure (cost + h, newState)

stochasticBeamSearch
  :: Int -- Hyperparameters
  -> Int
  -> SearchState' -- starting state
  -> (SearchState' -> ExceptT String IO [SearchState']) -- get adjacent states
  -> (SearchState' -> Bool) -- goal test
  -> ((SearchState', SearchState') -> ExceptT String IO Double) -- heuristic
  -> ExceptT String IO SearchState' -- output
stochasticBeamSearch beamWidth resevoir initialState getNextStates isGoalState heuristic = do
  gen <- lift initStdGen
  mgen <- lift $ newIOGenM gen
  search mgen $ heuristicSearchInit initialState heuristic 0 isGoalState
 where
  search mgen hs
    | null open = throwError "No Goal Found"
    | not $ null goalStates = pure . snd . head $ goalStates
    | otherwise = do
        -- lift $ Log.log $ T.pack (show open)

        nextStatesAll <- mapM
          (\(oldcost, s) -> do
            n <- getNextStates s
            pure $ map ((oldcost,s),) n
          ) open

        let allNextStates = concat nextStatesAll
        let l = length allNextStates
        -- Take a sample from list instead of all of them 
        -- truncated <- pure $ take resevoir allNextStates
        truncated <- lift $ reservoirSample mgen resevoir allNextStates
        nextWithCosts <- mapM doHeuristic truncated

        let nextStates = minElems beamWidth [] nextWithCosts
        search mgen $
          hs{frontier = nextStates}
   where
    getNextStatesWithPrev state = do
      next <- getNextStates state
      pure $ (state,) <$> next

    open = frontier hs
    goalStates = filter (isGoalState . snd) open

    minElems 0 mins [] = mins
    minElems 0 (m : mins) (x : rst)
      | fst x < fst m = minElems 0 (ins x mins) rst
      | otherwise = minElems 0 (m : mins) rst
    minElems n mins (x : rst) = minElems (n - 1) (ins x mins) rst
    minElems n mins [] = mins

    ins = L.insertBy ((flip . comparing) fst)

    doHeuristic ((oldcost, prev), curr) = do
      cost <- heuristic (prev,curr)
      pure (oldcost + cost, curr)

    getNextStatesAndCosts (cost, state) = do
      nextStates <- getNextStates state
      mapM go nextStates
     where
      go newState = do
        h <- heuristic (state, newState)
        pure (cost + h, newState)


-- | Entry point to the search algorithm
beamSearch
  :: Int -- Hyperparameters
  -> SearchState' -- starting state
  -> (SearchState' -> ExceptT String IO [SearchState']) -- get adjacent states
  -> (SearchState' -> Bool) -- goal test
  -> ((SearchState', SearchState') -> ExceptT String IO Double) -- heuristic
  -> ExceptT String IO SearchState' -- output
beamSearch beamWidth initialState getNextStates isGoalState heuristic = search $ heuristicSearchInit initialState heuristic 0 isGoalState
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
        h <- heuristic (state, newState)
        pure (cost + h, newState)

-- -- | Entry point to the search algorithm
-- heuristicSearch
--   :: Int -- Hyperparameters
--   -> SearchState' -- starting state
--   -> (SearchState' -> ExceptT String IO [SearchState']) -- get adjacent states
--   -> (SearchState' -> Bool) -- goal test
--   -> ((Maybe SearchState', SearchState') -> ExceptT String IO Double) -- heuristic
--   -> ExceptT String IO SearchState' -- output
-- heuristicSearch beamWidth initialState getNextStates isGoalState heuristic = do
--   initCost <- heuristic (Nothing, initialState)
--   search $ heuristicSearchInit initialState heuristic initCost isGoalState
--  where
--   search hs
--     | null open = throwError "No Goal Found"
--     | not $ null goalStates = pure . snd . head $ goalStates
--     | otherwise = do
--         lift $ Log.log $ T.pack (show open)
--
--         nextStates <- foldlM getNextStatesAndCosts [] open
--         let nextUnfreezeStates = minElems 1 [] $ filter (isFreeze . snd) nextStates
--             nextUnspreadStates = minElems beamWidth [] $ filter (isSpread . snd) nextStates
--             nextUnsplitStates = minElems beamWidth [] $ filter (isSplit . snd) nextStates
--             newFrontier = nextUnfreezeStates <> nextUnsplitStates
--         search $
--           hs{frontier = newFrontier}
--    where
--     open = frontier hs
--     goalStates = filter (isGoalState . snd) open
--
--     minElems 0 mins [] = mins
--     minElems 0 (m : mins) (x : rst)
--       | fst x < fst m = minElems 0 (ins x mins) rst
--       | otherwise = minElems 0 (m : mins) rst
--     minElems n mins (x : rst) = minElems (n - 1) (ins x mins) rst
--     minElems n mins [] = mins
--
--     ins = L.insertBy ((flip . comparing) fst)
--
--     getNextStatesAndCosts xs (cost, state) = do
--       nextStates <- getNextStates state
--       mapM go nextStates
--      where
--       go newState = do
--         h <- heuristic (Just state, newState)
--         pure (cost + h, newState)
--
--     isFreeze :: SearchState' -> Bool
--     isFreeze state = case getOpFromState state of
--       Nothing -> False
--       Just op -> opType op == Freeze'
--
--     isSplit :: SearchState' -> Bool
--     isSplit state = case getOpFromState state of
--       Nothing -> False
--       Just op -> opType op == Split'
--
--     isSpread :: SearchState' -> Bool
--     isSpread state = case getOpFromState state of
--       Nothing -> False
--       Just op -> opType op == Spread'
--
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
