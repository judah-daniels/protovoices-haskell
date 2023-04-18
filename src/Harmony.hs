{- | This module contains
  -}
module Harmony
  (
    evaluateSlice
  , mostLikelyLabelFromSliceWithProb
  , mostLikelyLabelFromSlice
  , labelLikelihoodGivenSlice
  , allLabelLikelihoodsGivenSlice
  , transposeSlice
  -- , ornamentLogLikelihoodDouble
  -- , sliceChordLogLikelihood
  -- , sliceChordWeightedLogLikelihoods
  -- , sliceChordWeightedLogLikelihood
  , ornamentLogLikelihood
  , chordToneLogLikelihood
  -- , scoreSegment
  , scoreSegment'
  , scoreSegments
  )
  where

import Probability
import Harmony.Params
import Harmony.ChordLabel
import Common
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Maybe
import Data.Ord
import Debug.Trace
import GHC.Float (double2Int)
import GHC.Generics
import GHC.Real (infinity)
import Internal.MultiSet qualified as MS
import Musicology.Core (AdditiveGroup)
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled
import Numeric.Log (Log (..))
import PVGrammar
import System.Random.MWC.Probability (multinomial, categorical)
import Data.Vector qualified as V



transposeNote :: Music.Pitch SIC -> SPitch -> SIC
transposeNote root = Music.pto root . spc . fifths

transposeSlice :: Music.Pitch SIC -> Notes SPitch -> Notes SIC
transposeSlice root (Notes slice) = Notes $ MS.map (transposeNote root) slice

scoreSegment' :: Notes SPitch -> ChordLabel -> Double
scoreSegment' (Notes slc) (ChordLabel chordType rootNote) = mlp + clp
 where
  slc' = Notes $ MS.map (transposeNote rootNote) slc
  valueVector = genSliceVector slc'
  chordTypeIndex = fromEnum chordType
  pChordTones = chordToneParams V.! chordTypeIndex
  mlp = (multinomialLogProb valueVector <$> chordToneParams) V.! chordTypeIndex
  clp = categoricalLogProb chordTypeIndex pChordTones

scoreSegments
  :: [Notes SPitch]
  -> [ChordLabel]
  -> Double
scoreSegments segments labels =
  let
    scores = zipWith scoreSegment' segments labels
   in
    sum scores / fromIntegral (length scores)


-- | Returns the most likely chord labels for each input group of notes
guessLabels :: [Notes SPitch] -> [ChordLabel]
guessLabels slices = mostLikelyLabelFromSlice <$> slices

mostLikelyLabelFromSlice :: Notes SPitch -> ChordLabel
-- mostLikelyLabelFromSlice slice = argmax allLabelLikelihoodsGivenSlice
  -- where 
    -- argmax :: V.Vector (Double, ChordLabel) -> ChordLabel
    -- argmax = V.foldl1' (\acc )
mostLikelyLabelFromSlice slice = argmax (`labelLikelihoodGivenSlice` slice) allChordLabels
  where
    argmax :: (a -> Double) -> V.Vector a -> a
    argmax f = V.foldl1' (\acc x -> if f x > f acc then x else acc)

mostLikelyLabelFromSliceWithProb :: Notes SPitch -> (ChordLabel, Double)

-- mostLikelyLabelFromSlice slice = argmax allLabelLikelihoodsGivenSlice
  -- where 
    -- argmax :: V.Vector (Double, ChordLabel) -> ChordLabel
    -- argmax = V.foldl1' (\acc )
mostLikelyLabelFromSliceWithProb slice = let l = argmax (`labelLikelihoodGivenSlice` slice) allChordLabels
  in
    (l, labelLikelihoodGivenSlice l slice)
  where
    argmax :: (a -> Double) -> V.Vector a -> a
    argmax f = V.foldl1' (\acc x -> if f x > f acc then x else acc)

allLabelLikelihoodsGivenSlice :: Notes SPitch -> V.Vector (Double, ChordLabel)
allLabelLikelihoodsGivenSlice slice =
  V.zip (V.map (`labelLikelihoodGivenSlice` slice) allChordLabels) allChordLabels
  -- where
  --   argmax :: (a -> Double) -> V.Vector a -> a
  --   argmax f = V.foldl1' (\acc x -> if f x > f acc then x else acc)


labelLikelihoodGivenSlice :: ChordLabel -> Notes SPitch -> Double
labelLikelihoodGivenSlice chordLabel@(ChordLabel chordType rootNote) slice
  = sliceLikelihoodGivenLabel + labelLikelihood
  where
    sliceVector = genSliceVector (transposeSlice rootNote slice)
    sliceLikelihoodGivenLabel = multinomialLogProb sliceVector (chordToneParams V.! fromEnum chordType)
    labelLikelihood = categoricalLogProb (fromEnum chordType) labelParams

allIntervals :: V.Vector SIC
allIntervals = V.fromList $ map sic [-14 .. 14]

allNotes :: V.Vector SPC
allNotes = V.fromList $ map spc [-14 .. 14]

allChordLabels :: V.Vector ChordLabel
allChordLabels = V.fromList $ do
    chordType <- [minBound..maxBound]
    rootNote <- map spc [-14 .. 14]
    pure $ ChordLabel chordType rootNote

-- | Provides a score measuring how much the slice matches the chord annoation
evaluateSlice :: Notes SIC -> ChordType -> Double
evaluateSlice pitchClasses chordType =
    likelihoods V.! fromEnum chordType
 where
  -- Calculate Likelihoods of each chord type
  valueVector = genSliceVector pitchClasses
  likelihoods = multinomialLogProb valueVector <$> chordToneParams

genMixture :: V.Vector Double -> V.Vector Double -> V.Vector Double
genMixture vec1 vec2 = (/ 2) <$> V.zipWith (+) vec1 vec2

chordToneLogLikelihood :: ChordLabel -> SPitch -> Double
chordToneLogLikelihood lbl@(ChordLabel chordType rootNote) note = logLikelihood
  where
  logLikelihood = categoricalLogProb notePos pChordTones
  pChordTones = chordToneParams V.! fromEnum chordType
  notePos = 14 + sFifth (transposeNote rootNote note)

ornamentLogLikelihood :: ChordLabel -> SPitch -> Double
ornamentLogLikelihood lbl@(ChordLabel chordType rootNote) note = logLikelihood
  where
  logLikelihood = categoricalLogProb notePos pChordTones
  pChordTones = ornamentParams V.! fromEnum chordType
  notePos = 14 + sFifth (transposeNote rootNote note)

-- ornamentLogLikelihoodDouble :: ChordLabel -> ChordLabel -> SPitch -> Double
-- ornamentLogLikelihoodDouble lbll@(ChordLabel chordTypel rootl) lblr@(ChordLabel chordTyper rootr) note = logLikelihood
 -- where
  -- logLikelihood = categoricalLogProb notePos pOrnamentsm
  -- logLikelihood = (ornamentLogLikelihood lbll (transposeNote rootl note) + ornamentLogLikelihood lblr (transposeNote rootr note)) / 2

genSliceVector :: Notes SIC -> V.Vector Double
genSliceVector (Notes notes)
  | sum res == fromIntegral (MS.size notes) =
      -- trace ("Slice Vector: \n" <> show (zip [-14 ..] (map double2Int res)))
      res
  -- Return empty list if any of the intervals are out of bound (eg. dddd4)
  | otherwise = V.replicate 29 0
 where
  res = myF <$> allIntervals :: V.Vector Double
  myF i = fromIntegral $ MS.lookup i notes

