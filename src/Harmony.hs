{- | This module contains
  -}
module Harmony
  (
    evaluateSlice
  , rotateVector
  , pChordTones
  , mostLikelyLabelFromSliceWithProb
  , probsRegs
  , probsPassings
  , probsFromLeft
  , probsFromRight
  , probsParent
  , mostLikelyLabelFromSlice
  , labelLikelihoodGivenSlice
  , allLabelLikelihoodsGivenSlice
  , transposeSlice
  , transposeNote
  , notePos
  , notesVector
  , noteVector
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
import Internal.MultiSet qualified as MS
import Musicology.Core (AdditiveGroup)
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled
import Numeric.Log (Log (..))
import PVGrammar
import System.Random.MWC.Probability (multinomial, categorical)
import Data.Vector qualified as V
import Data.Bifunctor (bimap)
import Data.Tuple (swap)
import Data.List



transposeNote :: Music.Pitch SIC -> SPitch -> SIC
transposeNote root = Music.pto root . spc . fifths

transposeSlice :: Music.Pitch SIC -> Notes SPitch -> Notes SIC
transposeSlice root (Notes slice) = Notes $ MS.map (transposeNote root) slice

scoreSegment'' :: Notes SPitch -> ChordLabel -> Double
scoreSegment'' (Notes slc) (ChordLabel chordType rootNote) = mlp + clp
 where
  slc' = Notes $ MS.map (transposeNote rootNote) slc
  valueVector = genSliceVector slc'
  chordTypeIndex = fromEnum chordType
  pChordTones = chordToneParams V.! chordTypeIndex
  mlp = (multinomialLogProb valueVector <$> chordToneParams) V.! chordTypeIndex
  clp = fromJust $ categoricalLogProb chordTypeIndex pChordTones

scoreSegment' :: Notes SPitch -> ChordLabel -> Double
scoreSegment' (Notes slc) (ChordLabel chordType rootNote) = mlp + clp
 where
  slc' = Notes $ MS.map (transposeNote rootNote) slc
  valueVector = genSliceVector slc'
  chordTypeIndex = fromEnum chordType
  -- pChordTones = chordToneParams V.! chordTypeIndex
  mlp = (multinomialLogProb valueVector <$> chordToneParams) V.! chordTypeIndex
  clp = fromJust $ categoricalLogProb (fromEnum chordType) labelParams

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
mostLikelyLabelFromSlice slice = snd . maximum $ V.toList $ allLabelLikelihoodsGivenSlice slice
  -- argmax (`labelLikelihoodGivenSlice` slice) allChordLabels
  -- where
  --   argmax :: (a -> Double) -> V.Vector a -> a
  --   argmax f = V.foldl1' (\acc x -> if f x > f acc then x else acc)

mostLikelyLabelFromSliceWithProb :: Notes SPitch -> (ChordLabel, Double)

-- mostLikelyLabelFromSlice slice = argmax allLabelLikelihoodsGivenSlice
  -- where 
    -- argmax :: V.Vector (Double, ChordLabel) -> ChordLabel
    -- argmax = V.foldl1' (\acc )
mostLikelyLabelFromSliceWithProb slice = swap . maximum $ V.toList $ allLabelLikelihoodsGivenSlice slice
-- let l = argmax (`labelLikelihoodGivenSlice` slice) allChordLabels
--   in
--     (l, labelLikelihoodGivenSlice l slice)
--   where
--     argmax :: (a -> Double) -> V.Vector a -> a
--     argmax f = V.foldl1' (\acc x -> if f x > f acc then x else acc)

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
    sliceVector =  notesVector slice
    sliceLikelihoodGivenLabel = case pChordTones chordLabel of
                                  Just p -> multinomialLogProb sliceVector p
                                  Nothing -> -1000
    labelLikelihood = fromJust $ categoricalLogProb (fromEnum chordType) labelParams

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

-- FOR HEURISTIC

genMixture :: V.Vector Double -> V.Vector Double -> V.Vector Double
genMixture vec1 vec2 = (/ 2) <$> V.zipWith (+) vec1 vec2

pChordTones (ChordLabel chordType rootNote) =
  let profile = chordToneParams V.! fromEnum chordType in
      rotateVector (fifths rootNote) profile
      -- Just profile


pOrnaments :: ChordLabel -> Maybe (V.Vector Double)
pOrnaments (ChordLabel chordType rootNote) =
  let profile = ornamentParams V.! fromEnum chordType in
      rotateVector (fifths rootNote) profile
      -- Just profile

probsRegs :: Maybe ChordLabel -> Maybe ChordLabel -> DoubleOrnament -> Maybe (V.Vector Double)
probsRegs slcL slcR orn
    -- Chord tone mixture on both sies
  | isRepetitionOnLeft orn && isRepetitionOnRight orn =
    case (slcL, slcR) of
      (Just lblL, Just lblR)
        -> mixture (pChordTones lblL) (pChordTones lblR)
      _ -> Nothing
    -- Chord tone on right, ornament of left
  | isRepetitionOnRight orn =
    case (slcL, slcR) of
      (Just lblL, Just lblR) -> mixture (pOrnaments lblL) (pChordTones lblR)
      (Just lblL , _) ->  pChordTones lblL
      (_ , Just lblR) ->  pOrnaments lblR
      (_ , _) -> Nothing
    -- Chord tone on right, ornament of left
  | isRepetitionOnLeft orn =
    case (slcL, slcR) of
      (Just lblL, Just lblR) -> mixture (pOrnaments lblR) (pChordTones lblL)
      (Just lblL , _) ->  pOrnaments lblL
      (_ , Just lblR) ->  pChordTones lblR
      (_ , _) -> Nothing
    -- Chord tone on left, ornament of right
  | otherwise = Nothing

probsPassings :: Maybe ChordLabel -> Maybe ChordLabel -> PassingOrnament -> Maybe (V.Vector Double)
probsPassings (Just lblL) (Just lblR) PassingMid = mixture (pOrnaments lblL) (pOrnaments lblR)
probsPassings (Just lblL) _ PassingLeft = pOrnaments lblL
probsPassings _ (Just lblR) PassingRight = pOrnaments lblR
probsPassings _ _ _  =  Nothing

probsFromLeft :: Maybe ChordLabel -> RightOrnament -> Maybe (V.Vector Double)
probsFromLeft Nothing _ =  Nothing
probsFromLeft (Just lbl) RightRepeat = pChordTones lbl
probsFromLeft (Just lbl) RightNeighbor = pOrnaments lbl

probsFromRight :: Maybe ChordLabel -> LeftOrnament -> Maybe (V.Vector Double)
probsFromRight Nothing _ =  Nothing
probsFromRight (Just lbl) LeftRepeat =  pChordTones lbl
probsFromRight (Just lbl) LeftNeighbor =  pOrnaments lbl

probsParent :: Maybe ChordLabel -> Maybe (V.Vector Double)
probsParent Nothing = Nothing
probsParent (Just lbl) = pChordTones lbl


-- Rotates a vector left might have to right idk lets see top tip DONT USE CHATGPT FOR SIMPLE CODE
rotateVector :: Int -> V.Vector a -> Maybe (V.Vector a)
rotateVector n xs
  | n == 0 = Just xs
  | n < 0 = Just $ V.fromList $ rotate' (-n) (V.toList xs)
  | n > 0 = Just $ V.fromList . reverse $ rotate' n (reverse $ V.toList xs)
  | otherwise = Nothing
  where
    l = length xs

rotate' n x =
   uncurry (flip (++))
           (splitAt (mod n (length x)) x)
-- rotate n x =
--    Match.take x (drop (mod n (length x)) (cycle x))
  -- | n <= V.length xs && n>= 0 = Just $ go n xs
  -- | n >= - V.length xs && n<= 0 = Just $ V.reverse $ go n $ V.reverse xs   -- this is cursed sorry lord
  -- | otherwise = Nothing
  -- where 
  --   go n xs = V.concat [V.slice n (V.length xs - n) xs, V.slice 0 n xs]
-- shiftVector :: Int -> V.Vector a -> V.Vector a 
-- shiftVector n x' = 
--   let x = V.toList x'
--    in
--      V.fromList $ Match.take x (drop (mod n (length x)) (cycle x))


    -- Chord tone on left, ornament of right
    --
chordToneLogLikelihood :: ChordLabel -> SPitch -> Double
chordToneLogLikelihood lbl@(ChordLabel chordType rootNote) note = logLikelihood
  where
  logLikelihood = fromJust $ categoricalLogProb notePos pChordTones
  pChordTones = chordToneParams V.! fromEnum chordType
  notePos = 14 + sFifth (transposeNote rootNote note)

ornamentLogLikelihood :: ChordLabel -> SPitch -> Double
ornamentLogLikelihood lbl@(ChordLabel chordType rootNote) note = logLikelihood
  where
  logLikelihood = fromJust $ categoricalLogProb notePos pChordTones
  pChordTones = ornamentParams V.! fromEnum chordType
  notePos = 14 + sFifth (transposeNote rootNote note)

-- ornamentLogLikelihoodDouble :: ChordLabel -> ChordLabel -> SPitch -> Double
-- ornamentLogLikelihoodDouble lbll@(ChordLabel chordTypel rootl) lblr@(ChordLabel chordTyper rootr) note = logLikelihood
 -- where
  -- logLikelihood = categoricalLogProb notePos pOrnamentsm
  -- logLikelihood = (ornamentLogLikelihood lbll (transposeNote rootl note) + ornamentLogLikelihood lblr (transposeNote rootr note)) / 2


notePos :: SPitch -> Maybe Int
notePos n
  | fifths n >= -14 && fifths n <= 14 = Just $ 14 + fifths n
  | otherwise  = Nothing


noteVector :: SPitch -> Maybe (V.Vector Double)
noteVector n = do
  p <- notePos n
  pure $ V.replicate 29 0 V.// [(p, 1)]

notesVector :: Notes SPitch -> V.Vector Double
notesVector (Notes ns) = V.replicate 29 0 V.// counts
  where
    -- counts = map (\ (a,b)-> ((fromJust . notePos $ a),(fromIntegral b))) $ filter (\(i,n)-> isJust (notePos i)) (MS.toOccurList ns)
    filtered = MS.filter (isJust . notePos) ns
    transformed = MS.map (fromJust . notePos) filtered
    counts = (\(a,b) -> (a, fromIntegral b))<$> MS.toOccurList transformed

-- -- SPELLED PITCH
-- noteVector :: SPitch -> V.Vector Double
-- noteVector (Notes notes)
--   | sum res == fromIntegral (MS.size notes) =
--       -- trace ("Slice Vector: \n" <> show (zip [-14 ..] (map double2Int res)))
--       res
--   -- Return empty list if any of the intervals are out of bound (eg. dddd4)
--   | otherwise = V.replicate 29 0
--  where
--   res = myF <$>  :: V.Vector Double
--   myF i = fromIntegral $ MS.lookup i notes
-- genNotesVector :: Notes SPitch -> V.Vector Double
-- genNotesVector (Notes notes)
--   | sum res == fromIntegral (MS.size notes) =
--       -- trace ("Slice Vector: \n" <> show (zip [-14 ..] (map double2Int res)))
--       res
--   -- Return empty list if any of the intervals are out of bound (eg. dddd4)
--   | otherwise = V.replicate 29 0
--  where
--   res = myF <$>  :: V.Vector Double
--   myF i = fromIntegral $ MS.lookup i notes

-- INTERVALS
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

