{-# LANGUAGE DeriveGeneric #-}
module Evaluator where

import Common
import GHC.Generics

import Numeric.SpecFunctions (logGamma)

import PVGrammar hiding
  ( slicesFromFile
  )

import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled

import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Lazy qualified as BL

import Data.List as List
import Data.Maybe
import Debug.Trace

import Internal.MultiSet (distinctElems,map, toOccurList, lookup, empty)

import System.Random.MWC.Probability (multinomial)
import Musicology.Core (AdditiveGroup)

data HarmonicProfileData = HarmonicProfileData
  { params :: Params
  , chordtypes :: [String]
  }
  deriving (Generic, Show)

instance FromJSON HarmonicProfileData

data Params = Params
  { params_p_harmony :: [Float]
  , params_p_chordtones :: [[Double]]
  , params_p_ornaments :: [[Float]]
  , alpha_p_ict :: [Float]
  , beta_p_ict :: [Float]
  , alpha_rate_notes :: Float
  , beta_rate_notes :: Float
  }
  deriving (Generic, Show)

instance FromJSON Params

loadParams :: FilePath -> IO HarmonicProfileData
loadParams file = do
  json <- BL.readFile "preprocessing/dcml_params.json"
  case (decode json :: Maybe HarmonicProfileData) of
    Just params -> pure params
    Nothing -> error "JSON parameter file not found or corrupted"


data ChordLabel = ChordLabel
  { chordType :: String 
  , rootOffset :: SIC
  , keyCenter :: SPC
  }
  deriving (Generic, Show)

-- Rename!!! evalSlice already exists
-- Path: sequences of alternating objects
-- ======================================

{- | Provides a score measuring how much the slice matches the chord annoation
    p
-}
evaluateSlice :: Notes SIC -> String -> HarmonicProfileData -> Double
evaluateSlice pitchClasses chordType hpData = trace 
  ("Evaluating Slice:" 
  <> "\n  Slice: " <> show pitchClasses 
  <> "\n  Label: " <> show chordType 
  <> "\n  Score: " <> show ( weightedlikelihoods !! chordTypeIndex))
  -- <> "\nWeighted Likelihoods: " <> showList ((zip (chordtypes hpData) weightedlikelihoods)) "")
    weightedlikelihoods !! chordTypeIndex 
  where 
    -- Calculate Likelihoods of each chord type 
    chordToneParams = getAllParams hpData

    valueVector = genSliceVector pitchClasses

    likelihoods = exp . multinomialLogProb valueVector <$> chordToneParams

    weightedlikelihoods = (/ maximum likelihoods) <$> likelihoods

    chordTypeIndex = fromMaybe 0 $ elemIndex chordType (chordtypes hpData)

genSliceVector :: Notes SIC -> [Double]
genSliceVector (Notes notes) = myF <$> [0 .. 28]
  where
    -- myF :: Integer -> Double
    myF i = fromIntegral $ Internal.MultiSet.lookup (sic (i-14)) notes 

getAllParams :: HarmonicProfileData -> [[Double]]
getAllParams hpData = normaliseList <$> pChordTones 
  where
    pChordTones = params_p_chordtones $ params hpData
    normaliseList xs = (/ sum xs) <$> xs

evalPath
  :: (Show ns, Music.Spelled ns)
  =>  Path (Edges ns) (Notes SPitch)
  -> [ChordLabel]
  -> HarmonicProfileData
  -> Double
evalPath (PathEnd _) _ _ = 0
evalPath (Path _ (Notes slc) rst) (lbl : lbls) hpData = evalSegment slc' lbl' hpData + evalPath rst lbls hpData
  where
    key = keyCenter lbl
    rOffset = rootOffset lbl
    lbl' = chordType lbl
    chordRootNote = key Music.+^ rOffset
    -- Input is SPC - Spelled Pitch Class eg F4
    -- key Is a SPC
    -- rOffset is an SIC (interval)
    -- chordRootNote is SPC

    slc' = Notes $ Internal.MultiSet.map transformPitch (slc)
      where
        transformPitch :: 
          Music.SPitch -> SIC
        transformPitch p = let q = spc (fifths p) in Music.pfrom q chordRootNote

evalSegment
  :: Notes SIC
  -> String
  -> HarmonicProfileData 

  -> Double
evalSegment (Notes ms) = evaluateSlice slice' 
  where
    slice' = Notes ms

multinomialLogProb :: [Double] -> [Double] -> Double
multinomialLogProb xs probs = logFactorialN + logPowers 
    where
      n = sum xs 
      logFactorialN = logGamma $ n + 1
      logPowers = sum $ zipWith powers xs probs
        where 
          powers x y = x * log y - logGamma (x + 1)
