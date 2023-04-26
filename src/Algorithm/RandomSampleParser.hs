{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Algorithm.RandomSampleParser
  (
    randomSamplePath
  , randomSamplePathSBS
  , perfectReduction
  , poissonSample
  )
    where


import Common ( Path (..) )
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import FileHandling (InputSlice)
import HeuristicParser (Slice, Trans)
import Musicology.Pitch ( spelledp, SPitch )
import PVGrammar ( Edges (..), Notes(..) )
import Data.Vector qualified as V
import Data.HashSet as S ( empty )
import Data.Heap qualified as H
import Data.List qualified as L
import Data.Maybe (fromJust, fromMaybe)

import Internal.MultiSet qualified as MS

import Probability
import Data.Aeson.KeyMap (singleton)
import System.Random (initStdGen)
import System.Random.Stateful
  ( StatefulGen
  , newIOGenM
  , uniformRM
  )
import Harmony.ChordLabel
import Harmony
import Musicology.Core

{- |
  Generates a path of random notes for the given number of segments
-}
randomSamplePath
  :: Int
  -> IO (Path (Edges SPitch) (Notes SPitch))
randomSamplePath numSegments = do
  gen <- initStdGen
  mgen <- newIOGenM gen
  genPath mgen numSegments
 where
  genPath mgen 0 = pure $ PathEnd (Edges S.empty MS.empty)
  genPath mgen n = do
    rst <- genPath mgen (n - 1)
    slc <- genSlice mgen
    pure $ Path (Edges S.empty MS.empty) slc rst
--
-- {- |
--   Generates a path of random notes from each segement for the given number of segments
-- -}
-- randomSamplePathSBS
--   :: [[InputSlice SPitch]]
--   -> IO (Path (Edges SPitch) (Notes SPitch))
-- randomSamplePathSBS inputSlices = do
--   gen <- initStdGen
--   mgen <- newIOGenM gen
--   genPath mgen inputSlices
--  where
--   genPath mgen [] = pure $ PathEnd (Edges S.empty MS.empty)
--   genPath mgen (seg : rst) = do
--     rst <- genPath mgen rst
--     slc <- genSliceSBS mgen seg
--     pure $ Path (Edges S.empty MS.empty) slc rst
--
randomSamplePathSBS
  :: [[InputSlice SPitch]]
  -> IO [Notes SPitch]
randomSamplePathSBS inputSlices = do
  gen <- initStdGen
  mgen <- newIOGenM gen
  mapM (genSliceSBS mgen) inputSlices

perfectReduction
  :: [[InputSlice SPitch]]
  -> [ChordLabel]
  -> IO [Notes SPitch]
perfectReduction  inputSlices chords = do
  gen <- initStdGen
  mgen <- newIOGenM gen
  mapM (genSlicePerfect mgen)  (zip inputSlices chords)

genSlicePerfect :: StatefulGen g IO => g -> ([InputSlice SPitch], ChordLabel) -> IO (Notes SPitch)
genSlicePerfect gen (slcs, lbl) = do
  let profile =  pChordTones lbl
  -- n <- uniformRM (2 :: Int, 5) gen
  let notes = filter (filterNote (lbl)) allNotes
  pure $ Notes $ MS.fromList notes
 where
  allNotes = fst <$> concatMap fst slcs

  -- filterNote Nothing _ = True
  filterNote lbl note = fifths (transposeNote rootNote' note) `elem` chordToneProfile chordType'
    where
      chordType' = chordType lbl
      rootNote' = rootNote lbl

      -- notePos = elem (14 + fifths note) (chordToneProfile lbl)

genSliceSBS :: StatefulGen g IO => g -> [InputSlice SPitch] -> IO (Notes SPitch)
genSliceSBS gen slcs = do
  -- n <- uniformRM (2 :: Int, 5) gen
  n <- poissonSample gen 5.2
  notes <- mapM (genNote gen allNotes) [1 .. (n+1)]
  pure $ Notes $ MS.fromList notes
 where
  allNotes = fst <$> concatMap fst slcs
  genNote gen allNotes _ = do
    mn <- pickRandom gen allNotes
    pure $ fromJust mn

genSlice :: StatefulGen g IO => g -> IO (Notes SPitch)
genSlice gen = do
  n <- uniformRM (2 :: Int, 5) gen
  notes <- mapM (genNote gen) [1 .. n]
  pure $ Notes $ MS.fromList notes
 where
  genNote gen _ = do
    i <- uniformRM (-7, 7) gen
    pure $ spelledp i i

