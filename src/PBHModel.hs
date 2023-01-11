module PBHModel
  -- TODO( HarmonicProfileData
  -- , Params
  -- , loadParams
  -- , ChordLabel 
  -- , evaluateSlice, mostLikelyChordFromSlice
  -- ) 
    where

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

data HarmonicProfileData = HarmonicProfileData
  { params :: Params
  , chordtypes :: [String]
  }
  deriving (Generic, Show)

instance FromJSON HarmonicProfileData

data Params = Params
  { params_p_harmony :: [Double]
  , params_p_chordtones :: [[Double]]
  , params_p_ornaments :: [[Double]]
  , alpha_p_ict :: [Double]
  , beta_p_ict :: [Double]
  , alpha_rate_notes :: Double
  , beta_rate_notes :: Double
  }
  deriving (Generic, Show)

instance FromJSON Params

-- | Load the paramaters from the JSON
loadParams :: FilePath -> IO HarmonicProfileData
loadParams file = do
  json <- BL.readFile "preprocessing/dcml_params.json"
  -- print json
  case (decode json :: Maybe HarmonicProfileData) of
    Just params -> pure params
    Nothing -> error "JSON parameter file not found or corrupted"

data ChordLabel = ChordLabel
  { chordType :: String
  , rootOffset :: SIC
  , keyCenter :: SPC
  }
  deriving (Generic, Show)

-- | Provides a score measuring how much the slice matches the chord annoation
evaluateSlice :: Notes SIC -> String -> HarmonicProfileData -> Double
evaluateSlice pitchClasses chordType hpData =
  trace
    ( "Evaluating Slice:"
        <> "\n  Slice: "
        <> show pitchClasses
        <> "\n  Label: "
        <> show chordType
        <> "\n  Score: "
        <> show (weightedlikelihoods !! chordTypeIndex)
    )
    -- <> "\nWeighted Likelihoods: " <> showList ((zip (chordtypes hpData) weightedlikelihoods)) "")
    weightedlikelihoods
    !! chordTypeIndex
 where
  -- Calculate Likelihoods of each chord type
  chordToneParams = getChordToneParams hpData

  valueVector = genSliceVector pitchClasses

  likelihoods = exp . multinomialLogProb valueVector <$> chordToneParams

  weightedlikelihoods = (/ maximum likelihoods) <$> likelihoods

  chordTypeIndex = fromMaybe 0 $ elemIndex chordType (chordtypes hpData)

-- SIC from C as a base
mostLikelyChordFromSlice :: HarmonicProfileData -> Notes SPitch -> (SIC, String, Double)
mostLikelyChordFromSlice hpData (Notes slc) = (sic root, chordtypes hpData !! chordTypeIndex, p)
 where
  (root, (chordTypeIndex, p)) = maximumBy (comparing (snd . snd)) (zip [0 ..] (mostLikelyChordType <$> sliceChordWeightedLogLikelihoods hpData notes))
   where
    mostLikelyChordType :: [Double] -> (Int, Double)
    mostLikelyChordType chordTypeProbs = maximumBy (comparing snd) (zip [0 ..] chordTypeProbs)
    notes = Notes $ MS.map transformPitch slc

transformSlice
  :: Notes Music.SPitch -> Notes SIC
transformSlice (Notes slc) = Notes $ MS.map transformPitch slc

transformPitch
  :: Music.SPitch -> SIC
transformPitch p = let q = spc (fifths p) in Music.pfrom q (spc 0)

maxi xs = maximumBy (comparing fst) (zip xs [0 ..])

-- Gives Likelihoods for all possible chord types in all root positions
-- Could ignore root positions which don't have a chord tone? Maybe
-- Assuming all are chordtones
sliceChordLogLikelihoods :: HarmonicProfileData -> Notes SIC -> [[Double]]
sliceChordLogLikelihoods hpData notes = map go [0 .. 11] -- map go [0..11]
 where
  go :: Int -> [Double]
  go root = map go' chordTypes
   where
    go' :: String -> Double
    go' lbl = sliceChordLogLikelihood hpData (ChordLabel lbl (sic root) (spc 0)) notes

  likelihoods = multinomialLogProb valueVector <$> pChordTones
  valueVector = genSliceVector notes
  pChordTones = getChordToneParams hpData
  chordTypes = chordtypes hpData

-- chordTypeIndex = fromMaybe 0 $ elemIndex undefined chordTypes

-- Gives Likelihoods for all possible chord types in all root positions
-- Could ignore root positions which don't have a chord tone? Maybe
-- Assuming all are chordtones
-- Scaled by prob of each chordtype
sliceChordWeightedLogLikelihoods :: HarmonicProfileData -> Notes SIC -> [[Double]]
sliceChordWeightedLogLikelihoods hpData notes = map go [0 .. 11] -- map go [0..11]
 where
  go :: Int -> [Double]
  go root = map go' chordTypes
   where
    go' :: String -> Double
    go' lbl = sliceChordWeightedLogLikelihood hpData (ChordLabel lbl (sic root) (spc 0)) notes

  likelihoods = multinomialLogProb valueVector <$> pChordTones
  valueVector = genSliceVector notes
  pChordTones = getChordToneParams hpData
  chordTypes = chordtypes hpData

-- chordTypeIndex = fromMaybe 0 $ elemIndex undefined chordTypes

sliceChordWeightedLogLikelihood :: HarmonicProfileData -> ChordLabel -> Notes SIC -> Double
sliceChordWeightedLogLikelihood hpData label notes = logLikelihood
 where
  logLikelihood = categoricalLogProb chordIndex pHarmony + multinomialLogProb valueVector pChordTones
  valueVector = genSliceVector notes
  pChordTones = getChordToneParams hpData !! chordIndex
  chordIndex = fromMaybe undefined (elemIndex (chordType label) chordTypes)
  pHarmony = getHarmonyParams hpData
  chordTypes = chordtypes hpData

sliceChordLogLikelihood :: HarmonicProfileData -> ChordLabel -> Notes SIC -> Double
sliceChordLogLikelihood hpData label notes = logLikelihood
 where
  logLikelihood = multinomialLogProb valueVector pChordTones
  valueVector = genSliceVector notes
  pChordTones = getChordToneParams hpData !! fromMaybe 0 (elemIndex (chordType label) chordTypes)
  chordTypes = chordtypes hpData

genSliceVector :: Notes SIC -> [Double]
genSliceVector (Notes notes) = myF <$> [0 .. 28]
 where
  myF i = fromIntegral $ MS.lookup (sic (i - 14)) notes

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

evalPath
  :: (Show ns, Music.Spelled ns)
  => Path (Edges ns) (Notes SPitch)
  -> [ChordLabel]
  -> HarmonicProfileData
  -> Double
evalPath (PathEnd _) _ _ = 0
evalPath (Path _ (Notes slc) rst) (lbl : lbls) hpData = evaluateSlice slc' lbl' hpData + evalPath rst lbls hpData
 where
  key = keyCenter lbl
  rOffset = rootOffset lbl
  lbl' = chordType lbl
  chordRootNote = key Music.+^ rOffset
  -- Input is SPC - Spelled Pitch Class eg F4
  -- key Is a SPC
  -- rOffset is an SIC (interval)
  -- chordRootNote is SPC

  slc' = Notes $ MS.map transformPitch slc
   where
    transformPitch
      :: Music.SPitch -> SIC
    transformPitch p = let q = spc (fifths p) in Music.pfrom q chordRootNote

-- Calculates the probability density of a multinomial distribution at the given point
multinomialLogProb :: [Double] -> [Double] -> Double
multinomialLogProb xs probs = logFactorialN + logPowers
 where
  n = sum xs
  logFactorialN = logGamma $ n + 1
  logPowers = sum $ zipWith powers xs probs
   where
    powers x y = x * log y - logGamma (x + 1)

-- Calculates the probability density of a multinomial distribution at the given point
categoricalLogProb :: Int -> [Double] -> Double
categoricalLogProb x probs = log $ probs !! x
