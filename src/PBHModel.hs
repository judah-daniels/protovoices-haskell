module PBHModel where

-- TODO( HarmonicProfileData
-- , Params
-- , loadParams
-- , ChordLabel
-- , evaluateSlice, mostLikelyChordFromSlice
-- )

import Common
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.List as List
import Data.Maybe
import Data.Ord
import Debug.Trace
import GHC.Generics
import Internal.MultiSet qualified as MS
import Musicology.Core (AdditiveGroup)
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled
import Numeric.Log (Log (..))
import Numeric.SpecFunctions (logGamma)
import PVGrammar
import System.Random.MWC.Probability (multinomial)
import GHC.Real (infinity)
import GHC.Float (double2Int)

data HarmonicProfileData = HarmonicProfileData
  { params :: Params,
    chordtypes :: [String]
  }
  deriving (Generic, Show)

instance FromJSON HarmonicProfileData

data Params = Params
  { params_p_harmony :: [Double],
    params_p_chordtones :: [[Double]],
    params_p_ornaments :: [[Double]],
    alpha_p_ict :: [Double],
    beta_p_ict :: [Double],
    alpha_rate_notes :: Double,
    beta_rate_notes :: Double
  }
  deriving (Generic, Show)

instance FromJSON Params

-- | Load the paramaters from the JSON
loadParams :: FilePath -> IO HarmonicProfileData
loadParams file = do
  json <- BL.readFile "preprocessing/dcml_params.json"
  -- print json
  case (decode json :: Maybe HarmonicProfileData) of
    Just hpData -> pure hpData'
      where
        hpData' =
          hpData
            { params = (params hpData) 
              { params_p_chordtones = 
                  (map . map) (+ 1) (params_p_chordtones (params hpData)),
                params_p_ornaments = 
                  (map . map) (+ 1) (params_p_ornaments (params hpData)),
                params_p_harmony = 
                  map (+ 1) (params_p_harmony (params hpData))
              }
            }
    Nothing -> error "JSON parameter file not found or corrupted"

data ChordLabel = ChordLabel
  { chordType :: String,
    rootNote :: SPC
  }
  deriving (Generic, Show, Eq)
    
mkLbl rootInt chordType = ChordLabel chordType (spc (rootInt-14))

-- Take the average score given a score function that takes slices and chord labels
scoreSegments 
  :: HarmonicProfileData 
  -> (Notes SPitch -> ChordLabel -> Double)
  -> [Notes SPitch]
  -> [ChordLabel]
  -> Double
scoreSegments hpData scoreSegment segments labels = 
  let 
    scores = zipWith scoreSegment segments labels 
  in 
    sum scores / fromIntegral (length scores)

-- | Provides a score measuring how much the slice matches the chord annoation
scoreSegment' :: HarmonicProfileData -> Notes SPitch -> ChordLabel -> Double
scoreSegment' hpData (Notes slc) (ChordLabel chordLbl root) = mlp + clp
  where
    -- slc' = Notes $ MS.map (\spitch' -> Music.pfrom (spc (fifths spitch')) root) slc
    slc' = Notes $ MS.map (transposeNote root)  slc
    -- Calculate Likelihoods of each chord type
    chordToneParams = getChordToneParams hpData

    valueVector = genSliceVector slc'

    mlp = (multinomialLogProb valueVector <$> chordToneParams ) !! chordTypeIndex

    clp = categoricalLogProb chordTypeIndex pChordTones

    pChordTones = getChordToneParams hpData !! chordTypeIndex

    chordTypeIndex = fromJust $ elemIndex chordLbl (chordtypes hpData)


transposeNote :: Music.Pitch SIC -> SPitch -> SIC
transposeNote root = Music.pto root . spc . fifths

-- | Provides a score measuring how much the slice matches the chord annoation
scoreSegment :: HarmonicProfileData -> Notes SPitch -> ChordLabel -> Double
scoreSegment hpData (Notes slc) (ChordLabel chordLbl root) = mlp
  where
    slc' = Notes $ MS.map (transposeNote root) slc

    -- Calculate Likelihoods of each chord type
    chordToneParams = getChordToneParams hpData

    valueVector = genSliceVector slc'

    mlp = (multinomialLogProb valueVector <$> chordToneParams ) !! chordTypeIndex

    chordTypeIndex = fromMaybe undefined $ elemIndex chordLbl (chordtypes hpData)

-- | Provides a score measuring how much the slice matches the chord annoation
evaluateSlice :: HarmonicProfileData -> Notes SIC -> String -> Double
evaluateSlice hpData pitchClasses chordType =
  trace
    ( "Evaluating Slice:"
        <> "\n  Slice: "
        <> show pitchClasses
        <> "\n  Label: "
        <> show chordType
        <> "\n  Score: "
        <> show (likelihoods !! chordTypeIndex)
    )
    -- <> "\nWeighted Likelihoods: " <> showList ((zip (chordtypes hpData) weightedlikelihoods)) "")
    -- weightedlikelihoods
    likelihoods
    !! chordTypeIndex
  where
    -- Calculate Likelihoods of each chord type
    chordToneParams = getChordToneParams hpData

    valueVector = genSliceVector pitchClasses

    likelihoods = multinomialLogProb valueVector <$> chordToneParams
    -- likelihoods' = exp <$> ((multinomialLogProb valueVector <$> chordToneParams))

    -- clp = categoricalLogProb chordTypeIndex pHarmony

    -- weightedlikelihoods = (/ maximum likelihoods) <$> likelihoods

    chordTypeIndex = fromMaybe undefined $ elemIndex chordType (chordtypes hpData)

-- SIC from C as a base
mostLikelyChordFromSlice :: HarmonicProfileData -> Notes SPitch -> (SPC, String, Double)
mostLikelyChordFromSlice hpData slc = (root, chordtypes hpData !! chordTypeIndex, p)
  where
    (p, root, chordTypeIndex) = maximum (sliceChordWeightedLogLikelihoods hpData slc)

-- notes = Notes $ MS.map transformPitch slc

-- transformSlice ::
--   Notes Music.SPitch -> Notes SIC

-- transformSlice r (Notes slc) = Notes $ MS.map (transformPitch r) slc

-- transformPitch ::
  -- Music.SPitch -> SIC
-- transformPitch p = let q = Music.pc p in Music.pfrom q (spc 0)

-- transformPitch :: Music.SPitch -> Music.Pitch p -> Music.ICOf p
-- transformPitch r p = Music.pfrom (Music.pc p) r

maxi xs = maximumBy (comparing fst) (zip xs [0 ..])

-- Gives Likelihoods for all possible chord types in all root positions
-- Could ignore root positions which don't have a chord tone? Maybe
-- Assuming all are chordtoneyyyyyyys

-- sliceChordLogLikelihoods :: HarmonicProfileData -> Notes SPitch -> [(Double, Int, Int)]
-- sliceChordLogLikelihoods hpData notes = allChords (map go [0 .. 28]) -- map go [0..11]
--   where
--     go :: Int -> [Double]
--     go root = map go' chordTypes
--       where
--         go' :: String -> Double
--         go' lbl = sliceChordLogLikelihood hpData (ChordLabel lbl (sic 0) (spc root)) notes'
--           where
--             notes' = transposeSlice (spc root) notes
--
--     chordTypes = chordtypes hpData

-- chordTypeIndex = fromMaybe 0 $ elemIndex undefined chordTypes

-- Gives Likelihoods for all possible chord types in all root positions
-- Could ignore root positions which don't have a chord tone? Maybe
-- Assuming all are chordtones
-- Scaled by prob of each chordtype

allChords :: [[Double]] -> [(Double, SPC, Int)]
allChords d = do
  (rootOffset, chordProbs) <- zip [0 ..] d
  (chordTypeIndex, chordProb) <- zip [0 ..] chordProbs
  pure (chordProb, spc (rootOffset - 14), chordTypeIndex)

-- chordTypeIndex = fromMaybe 0 $ elemIndex undefined chordTypes

-- (root, (chordTypeIndex, p)) = maximumBy (comparing (snd . snd)) (zip [0 ..] (mostLikelyChordType <$> sliceChordWeightedLogLikelihoods hpData slc))
--  where
--   mostLikelyChordType :: [Double] -> (Int, Double)
--   mostLikelyChordType chordTypeProbs = maximumBy (comparing snd) (zip [0 ..] chordTypeProbs)

sliceChordWeightedLogLikelihood :: HarmonicProfileData -> String -> Notes SIC -> Double
sliceChordWeightedLogLikelihood hpData label notes =
  -- trace
    -- ("\nSlice: " <> show notes <> "\nChord Prob: " 
    -- <> show logLikelihood <>
    --   "\nmlp:" <> show mlp
    -- <> "\nvector" <> show valueVector
    -- <> "slice Vec: " <> 
    -- )
    logLikelihood
  where
    -- myF i = fromIntegral $ MS.lookup (sic (i - 14)) notes
    clp = categoricalLogProb chordIndex pHarmony
    mlp = case multinomialLogProb valueVector pChordTones of 
            0 -> -100000
            x -> x
    logLikelihood = clp + mlp --- PARAMETER
    valueVector = genSliceVector notes
    pChordTones = getChordToneParams hpData !! chordIndex
    chordIndex = fromJust $ chordIndexFromLabel hpData label -- fromMaybe undefined (elemIndex label chordTypes)
    pHarmony = getHarmonyParams hpData
    chordTypes = chordtypes hpData

chordIndexFromLabel :: HarmonicProfileData -> String -> Maybe Int
chordIndexFromLabel hpData label = elemIndex label chordTypes
  where
    chordTypes = chordtypes hpData

sliceChordLogLikelihood :: HarmonicProfileData -> ChordLabel -> Notes SIC -> Double
sliceChordLogLikelihood hpData label notes = logLikelihood
  where
    logLikelihood = multinomialLogProb valueVector pChordTones
    valueVector = genSliceVector notes
    pChordTones = getChordToneParams hpData !! fromMaybe 0 (elemIndex (chordType label) chordTypes)
    chordTypes = chordtypes hpData

allIntervals = map sic [-14 .. 14]
allNotes = map spc [-14 .. 14]

sliceChordWeightedLogLikelihoods :: HarmonicProfileData -> Notes SPitch -> [(Double, SPC, Int)]
sliceChordWeightedLogLikelihoods hpData (Notes notes) = allChords (map likelihoodsGivenRootNote allNotes)
  where
    chordTypes = chordtypes hpData
    -- chordTypes = ["M"]
    -- chordtypes hpData

    -- root note from 0 to 28
    likelihoodsGivenRootNote :: SPC -> [Double]
    likelihoodsGivenRootNote root = 
      -- trace 
      -- ("Considering root note: " <> show root)
      map go chordTypes
      where
        go :: String -> Double
        go chordType = 
          -- trace 
          -- ("ChordType: " <> chordType <> "\n ll: " <> show ll) 
          ll
          where
            -- x = 
            ll =sliceChordWeightedLogLikelihood hpData chordType notes'
            notes' = 
              -- let y = 
                    -- trace ("Notes " <> show notes') undefined in 
                    Notes $ MS.map (transposeNote root ) notes
            -- notes' = transposeSlice (spc (root - 14)) notes

genSliceVector :: Notes SIC -> [Double]
genSliceVector (Notes notes) 
  | sum res == fromIntegral (MS.size notes) = 
    -- trace ("Slice Vector: \n" <> show (zip [-14 ..] (map double2Int res))) 
                res 
  -- Return empty list if any of the intervals are out of bound (eg. dddd4)
  | otherwise = replicate 29 0
  where
    res = myF <$> allIntervals :: [Double]
    myF i = fromIntegral $ MS.lookup i notes 

ornamentLogLikelihood :: HarmonicProfileData -> ChordLabel -> SIC -> Double
ornamentLogLikelihood hpData label note = logLikelihood
  where
    logLikelihood = categoricalLogProb notePos pOrnaments
    pOrnaments = getOrnamentParams hpData !! fromJust (chordIndexFromLabel hpData (chordType label))
    notePos = 14 + sFifth note

chordToneLogLikelihood :: HarmonicProfileData -> ChordLabel -> SIC -> Double
chordToneLogLikelihood hpData label note = logLikelihood
  where
    logLikelihood = categoricalLogProb notePos pChordTones
    pChordTones = getChordToneParams hpData !! fromMaybe undefined (chordIndexFromLabel hpData (chordType label))
    notePos = 14 + sFifth note

genMixture :: [Double] -> [Double] -> [Double]
genMixture vec1 vec2 = (/ 2) <$> zipWith (+) vec1 vec2

ornamentLogLikelihoodDouble :: HarmonicProfileData -> ChordLabel -> ChordLabel -> SPitch -> Double
ornamentLogLikelihoodDouble hpData lbll@(ChordLabel chordTypel rootl) lblr@(ChordLabel chordTyper rootr) note = logLikelihood
  where
    -- logLikelihood = categoricalLogProb notePos pOrnamentsm
    pOrnamentsl = getOrnamentParams hpData !! fromJust (chordIndexFromLabel hpData chordTypel)
    pOrnamentsr = getOrnamentParams hpData !! fromJust (chordIndexFromLabel hpData chordTyper)
    logLikelihood = (ornamentLogLikelihood hpData lbll (transposeNote rootl note) + ornamentLogLikelihood hpData lblr (transposeNote rootr note) ) / 2
    -- pOrnamentsm = genMixture pOrnamentsl pOrnamentsr
    -- notePos = 14 + sFifth note

chordToneLogLikelihoodDouble :: HarmonicProfileData -> ChordLabel -> ChordLabel -> SPitch -> Double
chordToneLogLikelihoodDouble hpData lbll@(ChordLabel chordTypel rootl) lblr@(ChordLabel chordTyper rootr) note = logLikelihood
  where
    pChordTonesl = getChordToneParams hpData !! fromJust (chordIndexFromLabel hpData chordTyper)
    pChordTonesr = getChordToneParams hpData !! fromJust (chordIndexFromLabel hpData chordTypel)
    logLikelihood = (chordToneLogLikelihood hpData lbll (transposeNote rootl note) + chordToneLogLikelihood hpData lblr (transposeNote rootr note) ) / 2

---- -- -- -- -- -- -- -- -- -- -- --
  -- EXTRACTING INFO FROM THE HARMONIC PROFILES
---- -- -- -- -- -- -- -- -- -- -- --

-- Takes the MLE estimate of the dirchetlet distribution
-- to get a categorical distribution for ornmanet probs...
getOrnamentParams :: HarmonicProfileData -> [[Double]]
getOrnamentParams hpData = normaliseList <$> pOrnaments
  where
    pOrnaments = params_p_ornaments $ params hpData

getChordToneParams :: HarmonicProfileData -> [[Double]]
getChordToneParams hpData = normaliseList <$> pChordTones
  where
    pChordTones = params_p_chordtones $ params hpData

-- MLE sample of dirchlet to get categorical distibution for haromy.
getHarmonyParams :: HarmonicProfileData -> [Double]
getHarmonyParams hpData = normaliseList pHarmony
  where
    pHarmony = params_p_harmony $ params hpData

normaliseList :: (Fractional a) => [a] -> [a]
normaliseList xs = (/ sum xs) <$> xs

---- -- -- -- -- -- -- -- -- -- -- --
  -- EXTRACTING INFO FROM THE HARMONIC PROFILES
---- -- -- -- -- -- -- -- -- -- -- --


-- evalPath ::
--   Path es (Notes SPitch) ->
--   [ChordLabel] ->
--   HarmonicProfileData ->
--   Double
-- evalPath (PathEnd _) _ _ = 0
-- evalPath _ [] _ = trace "WARNING: Chords don't line up with parsed slices." 0
-- evalPath (Path _ (Notes slc) rst) (lbl : lbls) hpData = trace (show slc' <> show lbl')  $ evaluateSlice hpData slc' lbl' + evalPath rst lbls hpData
--   where
--     key = keyCenter lbl
--     rOffset = rootOffset lbl
--     chordRootNote = key Music.+^ rOffset
--
--     lbl' = chordType lbl
--     slc' = Notes $ MS.map transformPitch slc
--       where
--         transformPitch ::
--           Music.SPitch -> SIC
--         transformPitch p = Music.pfrom (Music.pc p) chordRootNote
--

-- transposeSlice :: SPC -> Notes SPitch -> Notes SIC
-- transposeSlice root (Notes slc) = Notes $ MS.map transformPitch slc
--   where
--     transformPitch ::
--       SPitch -> SIC
--     transformPitch p = Music.pfrom (Music.pc p) root

-- Calculates the probability density of a multinomial distribution at the given point
multinomialLogProb :: [Double] -> [Double] -> Double
multinomialLogProb xs probs 
  | n == 0 = trace "empty multinomial" (- 100000000 )
  | otherwise = logFactorialN + logPowers
  where
    n = sum xs
    logFactorialN = logGamma $ n + 1
    logPowers = sum $ zipWith powers xs probs
      where
        powers x y = x * log y - logGamma (x + 1)

-- Calculates the probability density of a multinomial distribution at the given point
categoricalLogProb :: Int -> [Double] -> Double
categoricalLogProb x probs = log $ probs !! x
