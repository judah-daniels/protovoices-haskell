{- | This module contains
  -}
module Harmony
  (
    evaluateSlice
  , mostLikelyLabelFromSlice
  , labelLikelihoodGivenSlice
  , allLabelLikelihoodsGivenSlice
  , transposeSlice
  , multinomialLogProb
  , categoricalLogProb
  -- , ornamentLogLikelihood
  -- , ornamentLogLikelihoodDouble
  -- , sliceChordLogLikelihood
  -- , chordToneLogLikelihoodDouble
  -- , sliceChordWeightedLogLikelihoods
  -- , sliceChordWeightedLogLikelihood
  -- , scoreSegment
  , scoreSegment'
  -- , scoreSegments
  )
  where

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
import Numeric.SpecFunctions (logGamma)
import PVGrammar
import System.Random.MWC.Probability (multinomial)
import Data.Vector qualified as V
import PBHModel (mostLikelyChordFromSlice)


multinomialLogProb :: V.Vector Double -> V.Vector Double -> Double
multinomialLogProb xs probs
  | n == 0 = trace "empty multinomial" (-100000000)
  | otherwise = logFactorialN + logPowers
 where
  n = sum xs
  logFactorialN = logGamma $ n + 1
  logPowers = V.sum $ V.zipWith powers xs probs
   where
    powers x y = x * log y - logGamma (x + 1)

-- Calculates the probability density of a multinomial distribution at the given point
categoricalLogProb :: Int -> V.Vector Double -> Double
categoricalLogProb x probs = log $ probs V.! x

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


sliceChordWeightedLogLikelihood :: Notes SIC -> ChordLabel -> Double
sliceChordWeightedLogLikelihood slice label@(ChordLabel chordType rootNote)  =
  logLikelihood
 where
  clp = categoricalLogProb (fromEnum chordType) labelParams
  mlp = case multinomialLogProb valueVector pChordTones of
    0 -> -100000
    x -> x
  logLikelihood = clp + mlp
  valueVector = genSliceVector slice
  pChordTones = chordToneParams V.! fromEnum chordType


allIntervals = V.fromList $ map sic [-14 .. 14]

allNotes = V.fromList $ map spc [-14 .. 14]

-- allLabels = V.fromList $ do 
  -- chordType <- 

allChordLabels :: V.Vector ChordLabel
allChordLabels = V.fromList $ do
    chordType <- [minBound..maxBound]
    rootNote <- map spc [-14 .. 14] 
    pure $ ChordLabel chordType rootNote
-- allChordLabels = [ChordLabel chordType rootNote | chordType <- [minBound..maxBound], rootNote <- map spc [-14 .. 14]]

  -- map spc [-14 .. 14]

-- | Provides a score measuring how much the slice matches the chord annoation
evaluateSlice :: Notes SIC -> ChordType -> Double
evaluateSlice pitchClasses chordType =
    likelihoods V.! fromEnum chordType
 where
  -- Calculate Likelihoods of each chord type
  valueVector = genSliceVector pitchClasses
  likelihoods = multinomialLogProb valueVector <$> chordToneParams


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

