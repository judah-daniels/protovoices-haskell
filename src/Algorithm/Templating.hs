{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Algorithm.Templating
  (
    templatingBaseline
  )
    where


import Harmony.ChordLabel
import FileHandling (InputSlice)

import PVGrammar ( Edges (..), Notes(..) )

import Data.List qualified as L
import Internal.MultiSet qualified as MS

import Musicology.Core
import Data.Ord (comparing)
import Debug.Trace

templatingBaseline
  :: [[InputSlice SPitch]]
  -> ([Notes SPitch], [ChordLabel])
templatingBaseline = unzip . map genSlice

genSlice :: [InputSlice SPitch] -> (Notes SPitch, ChordLabel)
genSlice slc =
  let scores = zip (scoreTemplate slc <$> allChordLabels) allChordLabels
      (bestInt, bestLabel) = maximum scores
      best = trace ("\nSlice: " <> show slc <> "\n best:" <> show (filter ((bestInt ==) . fst) scores)) $ filter ((bestInt ==) . fst) scores
        -- maxElems (-1) [] scores
      bestone = breakTies best
  in
    (Notes $ MS.fromList $ (`spelledp` 0) . (fifths (rootNote bestone) +) <$> chordToneProfile (chordType bestone)
      , bestone )

  where
    breakTies scoresWithLabels = snd $ head scoresWithLabels
    -- 1. ROOT WEIGHT: Choose the template whose root pitch class has the greatest weight of notes present in the
    -- segment
    -- 2. PRIOR PROBABILITY: Choose the template with higher prior probability of occurence
    -- 3. DIM7 RESOLUTION: If al top templates are fully diminished 7th chords, select the template whose roott is one
    -- half-step below the root of the top scoring template in the following segment.

scoreTemplate
  :: [InputSlice SPitch]
  -> ChordLabel
  -> Int
scoreTemplate slcs (ChordLabel chordType rootNote)=  p - (m + n)
 where
    p = length $ filter (`elem` template) allNotes
    m = length $ filter (not . (`elem` template)) allNotes
    n = length $ filter (not . (`elem` allNotes)) template

    template = chordToneProfile chordType
    allNotes = (\x -> x - (fifths rootNote)) . fifths . fst <$> concatMap fst slcs
