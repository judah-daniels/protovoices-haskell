{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Algorithm.HeuristicSearch
  (
    -- heuristicSearch
    aab
      , stochasticSearch
    ,stochasticBeamSearch
    ,dualStochasticBeamSearch
    ,stochasticBeamSearchLimited
    , beamSearch
  )
    where

import Probability
import Control.Logging qualified as Log
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Foldable ( foldlM, maximumBy )
import Data.List qualified as L
import Data.Maybe (fromMaybe, maybeToList, catMaybes)
import Data.Ord ( comparing )
import Musicology.Core ( SPitch )
-- import List.Transformer
-- import Control.Foldl (purely)

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
import Control.Monad (replicateM)


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

stochasticSearch
  :: Int
  -> SearchState' -- starting state
  -> (SearchState' -> ExceptT String IO [SearchState']) -- get adjacent states
  -> (SearchState' -> Bool) -- goal test
  -> ((SearchState', SearchState') -> ExceptT String IO Double) -- heuristic
  -> ExceptT String IO SearchState' -- output
stochasticSearch beamWidth initialState getNextStates isGoalState heuristic = do
  gen <- lift initStdGen
  mgen <- lift $ newIOGenM gen
  search mgen $ heuristicSearchInit initialState heuristic 0 isGoalState
 where
  search mgen hs
    | null open = throwError "No Goal Found"
    | not $ null goalStates = do
        lift $ Log.log $ T.pack ("Goal States:\n"<> concatMap (\(a,b)-> show a <> show b <> "\n") goalStates)
        pure . snd . head $ minElems 1 [] (filter (isFreeze . snd ) goalStates)
    | otherwise = do
        lift $ Log.log $ T.pack ("\nBeam:\n"<> concatMap (\(a,b)-> show a <> show b <> "\n") open)
        nextStatesAll <- mapM
          (\(oldcost, s) -> do
            n <- getNextStates s
            res1 <- pickRandom mgen n
            res2 <- pickRandom mgen n
            res3 <- pickRandom mgen n
            let res = catMaybes [res1,res2,res3]
            pure $ map ((oldcost,s),) n
          ) open
        let allNextStatesWithNan = concat nextStatesAll
        logD $ "All states: " <> show (length allNextStatesWithNan)
        let allNextStates = filter (not . isNaN . fst . fst ) allNextStatesWithNan
        logD $ "All valid states: " <> show (length allNextStates)


        nextUnfreezeStates <- do
          let filtered = filter (isFreeze . snd) allNextStates
          logD $ "Unfreezes: " <> show (length filtered)
          r <- mapM doHeuristic filtered
          logD $ "Selected: " <> show (length r)
          pure $ filter (not . isNaN . fst) r
        nextUnspreadStates <- do
          let filtered = filter (isSpread . snd) allNextStates
          logD $ "Unspreads: " <> show (length filtered)
          -- x <- lift $ reservoirSample mgen reservoir filtered
          -- logD $ "Selected: " <> show (length x)
          r <- mapM doHeuristic filtered
          pure $ filter (not . isNaN . fst) r
        nextUnsplitStates <- do
          let filtered = filter (isSplit . snd) allNextStates
          logD $ "Unsplits: " <> show (length filtered)
          -- x <- lift $ reservoirSample mgen reservoir filtered
          -- logD $ "Unsplits: " <> show (length x)
          r <- mapM doHeuristic filtered
          pure $ filter (not . isNaN . fst) r

        let nextStates = concatMap (minElems beamWidth []) [nextUnfreezeStates,nextUnsplitStates,nextUnspreadStates]

        search mgen $
          hs{frontier = nextStates}
   where
    getNextStatesWithPrev state = do
      next <- getNextStates state
      pure $ (state,) <$> next

    open = frontier hs
    goalStates = filter (((&&) <$> isFreeze <*> isGoalState) . snd) open

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


dualStochasticBeamSearch
  :: Int -- Hyperparameters
  -> Int
  -> SearchState' -- starting state
  -> (SearchState' -> ExceptT String IO [SearchState']) -- get adjacent states
  -> (SearchState' -> Bool) -- goal test
  -> ((SearchState', SearchState') -> ExceptT String IO Double) -- heuristic
  -> ExceptT String IO SearchState' -- output
dualStochasticBeamSearch beamWidth reservoir initialState getNextStates isGoalState heuristic = do
  gen <- lift initStdGen
  mgen <- lift $ newIOGenM gen
  search mgen $ heuristicSearchInit initialState heuristic 0 isGoalState
 where
  search mgen hs
    | null open = throwError "No Goal Found"
    | not $ null goalStates = do
        lift $ Log.debug $ T.pack ("Goal States:\n"<> concatMap (\(a,b)-> show a <> show b <> "\n") goalStates)
        pure . snd . head $ minElems 1 [] (filter (isFreeze . snd ) goalStates)
    | otherwise = do
        lift $ Log.debug $ T.pack ("\nBeam:\n"<> concatMap (\(a,b)-> show a <> show b <> "\n") open)
        nextStatesAll <- mapM
          (\(oldcost, s) -> do
            n <- getNextStates s
            pure $ map ((oldcost,s),) n
          ) open
        let allNextStatesWithNan = concat nextStatesAll
        logD $ "All states: " <> show (length allNextStatesWithNan)
        let allNextStates = allNextStatesWithNan
        logD $ "All valid states: " <> show (length allNextStates)

        nextUnfreezeStates <- do
          let filtered = filter (isFreeze . snd) allNextStates
          logD $ "Unfreezes: " <> show (length filtered)
          r <- mapM doHeuristic filtered
          logD $ "Selected: " <> show (length r)
          pure $ r
        nextUnspreadStates <- do
          let filtered = filter (isSpread . snd) allNextStates
          logD $ "Unspreads: " <> show (length filtered)
          -- x <- lift $ reservoirSample mgen reservoir filtered
          -- logD $ "Selected: " <> show (length x)
          r <- mapM doHeuristic filtered
          pure $ r
        nextUnsplitStates <- do
          let filtered = filter (isSplit . snd) allNextStates
          logD $ "Unsplits: " <> show (length filtered)
          x <- lift $ reservoirSample mgen reservoir filtered
          logD $ "Unsplits: " <> show (length x)
          r <- mapM doHeuristic x
          pure $ r

        let nextStates = concatMap (minElems beamWidth []) [nextUnfreezeStates,nextUnsplitStates,nextUnspreadStates]

        search mgen $
          hs{frontier = nextStates}
   where
    getNextStatesWithPrev state = do
      next <- getNextStates state
      pure $ (state,) <$> next

    open = frontier hs
    goalStates = filter (((&&) <$> isFreeze <*> isGoalState) . snd) open

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


dualStochasticBeamSearch'
  :: Int -- Hyperparameters
  -> Int
  -> SearchState' -- starting state
  -> (SearchState' -> ExceptT String IO [SearchState']) -- get adjacent states
  -> (SearchState' -> Bool) -- goal test
  -> ((SearchState', SearchState') -> ExceptT String IO Double) -- heuristic
  -> ExceptT String IO SearchState' -- output
dualStochasticBeamSearch' beamWidth reservoir initialState getNextStates isGoalState heuristic = do
  gen <- lift initStdGen
  mgen <- lift $ newIOGenM gen
  search mgen $ heuristicSearchInit initialState heuristic 0 isGoalState
 where
  search mgen hs
    | null open = throwError "No Goal Found"
    | not $ null goalStates = do
        lift $ Log.log $ T.pack ("Goal States:\n"<> concatMap (\(a,b)-> show a <> show b <> "\n") goalStates)
        pure . snd . head $ minElems 1 [] (filter (isFreeze . snd ) goalStates)
    | otherwise = do
        -- Find neighboring states
        -- nextStates <- getNextStates nearestState
        lift $ Log.debug $ T.pack ("\nBeam:\n"<> concatMap (\(a,b)-> show a <> show b <> "\n") open)
        nextStatesAll <- mapM
          (\(oldcost, s) -> do
            n <- getNextStates s
            res <- replicateM reservoir $ pickRandom mgen n
            let t = map ((oldcost,s),) (catMaybes res)
            mapM doHeuristic t

          ) open

        let allNextStatesWithNan = concat nextStatesAll
        nextStates' <- replicateM beamWidth $ pickRandom mgen $ allNextStatesWithNan
        let nextStates'' = catMaybes nextStates'
        let nextStates = minElems beamWidth [] nextStates''

        search mgen $
          hs{frontier = nextStates}
   where
    getNextStatesWithPrev state = do
      next <- getNextStates state
      pure $ (state,) <$> next

    open = frontier hs
    goalStates = filter (((&&) <$> isFreeze <*> isGoalState) . snd) open

    minElems 0 mins [] = mins
    minElems 0 (m : mins) (x : rst)
      | fst x < fst m = minElems 0 (ins x mins) rst
      | otherwise = minElems 0 (m : mins) rst
    minElems n mins (x : rst) = minElems (n - 1) (ins x mins) rst
    minElems n mins [] = mins

    ins = L.insertBy ((flip . comparing) fst)

    doHeuristic ((oldcost, prev), curr) = do
      cost <- heuristic (prev,curr)
      if isNaN cost then do
                    lift $ Log.setLogLevel Log.LevelDebug
                    lift $ print "FUCKFUCKFUCK"
                    cost <- heuristic (prev,curr)
                    lift $ Log.setLogLevel Log.LevelInfo
                    else lift $ Log.setLogLevel Log.LevelInfo
                      -- lift $ print "ok"
                    -- print ("FUCKING NAN: " <> show curr) else lift $ print "ok"
      pure (oldcost + cost, curr)

    getNextStatesAndCosts (cost, state) = do
      nextStates <- getNextStates state
      mapM go nextStates
     where
      go newState = do
        h <- heuristic (state, newState)
        pure (cost + h, newState)

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
        lift $ Log.log $ T.pack (show open)
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

logD x = lift $ Log.debug $ T.pack x

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
        logD $ show open

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
    | not $ null goalStates = do
        lift $ Log.log $ T.pack (show goalStates)
        pure . snd . head $ goalStates
    | otherwise = do
        -- lift $ Log.log $ T.pack (show open)

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

data PvOp = Split' | Spread' | Freeze' deriving (Eq)


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
