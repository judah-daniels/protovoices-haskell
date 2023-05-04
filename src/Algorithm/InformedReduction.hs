{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm.InformedReduction 
  (informedReduction)
    where

import Musicology.Core
import Internal.MultiSet qualified as MS
import PVGrammar

import FileHandling 
import Harmony.ChordLabel
import Harmony

informedReduction
  :: [[InputSlice SPitch]]
  -> [ChordLabel]
  -> [Notes SPitch]
informedReduction inputSlices chords = genSlicePerfect <$> zip inputSlices chords
 where 
  filterNote lbl note = 
    let chordToneProfile' = chordToneProfile $ chordType lbl
        noteRelRoot = transposeNote (rootNote lbl) note
      in 
        fifths noteRelRoot `elem` chordToneProfile'

  genSlicePerfect (slcs, lbl) =
    let allNotes = fst <$> concatMap fst slcs
        notes = filter (filterNote lbl) allNotes
      in 
        Notes $ MS.fromList notes
