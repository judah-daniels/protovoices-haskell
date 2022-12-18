{-# LANGUAGE DeriveGeneric #-}

module Evaluator where 

import GHC.Generics

import Common

import PVGrammar hiding
  ( slicesFromFile,
  )
import PVGrammar.Generate
import PVGrammar.Parse

import qualified Musicology.Core as Music
import Musicology.Pitch.Spelled 

import Data.Aeson
import Data.Aeson.TH

import qualified Data.ByteString.Lazy as BL


data HarmonicProfileData = HarmonicProfileData
  { params :: Params
  , chordTypes :: [String]
  } deriving (Generic, Show)

instance FromJSON HarmonicProfileData

data Params = Params
  { paramsPHarmony :: [[Float]]
  , paramsPChordtones :: [[Float]]
  , paramsPOrnaments :: [[Float]]
  , alphaPIct :: [Float]
  , betaPIct :: [Float]
  } deriving (Generic, Show)

instance FromJSON Params


loadParams :: FilePath -> IO HarmonicProfileData
loadParams file = do
  json <- BL.readFile file
  case (decode json :: Maybe HarmonicProfileData) of 
    Just params -> pure params
    Nothing -> error "JSON parameter file not found or corrupted"


-- Rename!!! evalSlice already exists
evaluateSlice :: [Int] -> Int -> Float
evaluateSlice pitchClasses chordLabel = undefined
-- 
evalPath 
  :: Path (Edges SPC) (Notes SPC) 
  -> [String]
  -> Float
evalPath (PathEnd edge) xs = 1
evalPath (Path edge slc rst) (lbl:lbls) = evalSegment slc lbl + evalPath rst lbls

evalSegment
  :: Notes SPC 
  -> String
  -> Float
evalSegment = undefined
