{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

templatingBaseline
  :: [[InputSlice SPitch]]
  -> ([Notes SPitch], [ChordLabel])
templatingBaseline = unzip . map genSlice

genSlice :: [InputSlice SPitch] -> (Notes SPitch, ChordLabel)
genSlice slc =
  let scores = zip (scoreTemplate slc <$> allChordLabels) allChordLabels
      best = minElems (-1) [] scores
      bestone = breakTies best
  in
    (Notes $ MS.fromList $ (`spelledp` 0) <$> chordToneProfile (chordType bestone)
      , bestone )

  where
    breakTies scoresWithLabels = snd $ head scoresWithLabels 
    -- 1. ROOT WEIGHT: Choose the template whose root pitch class has the greatest weight of notes present in the
    -- segment
    -- 2. PRIOR PROBABILITY: Choose the template with higher prior probability of occurence
    -- 3. DIM7 RESOLUTION: If al top templates are fully diminished 7th chords, select the template whose roott is one
    -- half-step below the root of the top scorig template in the following segment.

    minElems 0 mins [] = mins
    minElems n mins [] = mins
    minElems 0 (m : mins) (x : rst)
      | fst x < fst m = minElems 0 (ins x mins) rst
      | otherwise = minElems 0 (m : mins) rst
    minElems n mins (x : rst) = minElems (n - 1) (ins x mins) rst

    ins = L.insertBy ((flip . comparing) fst)

scoreTemplate
  :: [InputSlice SPitch]
  -> ChordLabel
  -> Int
scoreTemplate slcs (ChordLabel chordType rootNote)=  p - (m + n)
 where
    p = length $ filter (`elem` template) allNotes
    m = length $ filter (not . (`elem` template)) allNotes
    n = length $ filter (not . (`elem` allNotes)) template

    template = sic <$> chordToneProfile chordType
    allNotes = pfrom rootNote . spc . fifths . fst <$> concatMap fst slcs
