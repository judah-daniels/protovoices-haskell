{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RandomSampleParser where

import Common
import Musicology.Pitch
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import HeuristicParser (Slice, Trans)
import PVGrammar

import Internal.MultiSet qualified as MS
import Data.Foldable
import Data.HashSet as S
import qualified Data.Heap as H
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Ord
import Debug.Trace

import Data.Aeson.KeyMap (singleton)
import System.Random (initStdGen)
import System.Random.Stateful
  ( StatefulGen
  , newIOGenM
  , uniformRM
  )

{- |
  Generates a path of random notes for the given number of segments
-}
randomSamplePath
  :: Int
  -> IO (Path (Edges SPitch) (Notes SPitch))
randomSamplePath numSegments = do
  gen <- initStdGen
  mgen <-  newIOGenM gen
  genPath mgen numSegments
 where
  genPath mgen 0 = pure $ PathEnd (Edges S.empty MS.empty)
  genPath mgen n = do 
    rst <- genPath mgen (n-1)
    slc <- genSlice mgen
    pure $ Path (Edges S.empty MS.empty) slc rst

genSlice :: StatefulGen g IO => g -> IO (Notes SPitch)
genSlice gen = do 
  n <- uniformRM (2::Int, 5) gen
  notes <- mapM (genNote gen)  [1..n]
  pure $ Notes $ MS.fromList notes
  where 
    genNote gen _ = do 
      i <- uniformRM (-7,7) gen
      pure $ spelledp i i
   


pickRandom :: StatefulGen g m => g -> [slc] -> m (Maybe slc)
pickRandom _ [] = pure Nothing
pickRandom gen xs = do
  i <- uniformRM (0, length xs - 1) gen
  pure $ Just $ xs !! i