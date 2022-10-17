{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- CSV Imports
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
-- Musicology imports

import qualified Musicology.Core as Music
import qualified Musicology.Core.Slicing as Music
import qualified Musicology.MusicXML as MusicXML
import Musicology.Pitch
  ( Interval,
    Notation (..),
    Pitch,
    SInterval,
    SPC,
    SPitch,
    pc,
  )
import Musicology.Pitch.Spelled as MT
import Data.Ratio

data Person = Person
  { name :: !String,
    salary :: !Int
  }

type IntervalType = (Ratio Int, Ratio Int)


instance FromField IntervalType where
  parseField s = pure (3,3)

-- Note
data SalamiNote = SalamiNote
  { onset_slice :: !IntervalType --(Ratio Int, Ratio Int)
  -- , _interval :: !String
    -- , quarterbeats :: Integer
    -- , duration_qb :: Float
    -- , mc :: Float
    -- , mn :: Float
    -- , mc_onset :: Float
    -- , mn_onset :: Float
    -- , timesig :: TL.Text
    -- , staff :: TL.Text
    -- , voice :: TL.Text
    -- , duration :: Float
    -- , gracenote :: Float
    -- , nominal_duration :: Float
    -- , scalar :: Float
    -- , tied :: Float
    -- , tpc :: Float
  , midi :: !Int 
  -- , _pitch :: SPitch
    -- , volta :: Float
  , segmentId :: !Int
  }


-- instance FromField (Ratio Int, Ratio Int) where

instance FromNamedRecord SalamiNote where
  parseNamedRecord r =
    SalamiNote
      <$> r .: "onset_slice"
      -- <*> r .: "_interval"
      <*> r .: "midi"
      -- <*> r .: "_pitch"
      <*> r .: "segment_id"



main :: IO ()
main = do
  csvData <- BL.readFile "preprocessing/salamis.csv" 
  case decodeByName csvData of
    Left err -> putStrLn err
    -- Right (_, v) -> putStrLn "nice"
    Right (_, v) -> V.forM_ v $ processSlice
      where
        processSlice p = do
          putStrLn $ show $ noteFromSalami p
          putStrLn $ "Slice: " ++ show ((midi p):: Int)  ++ " MIDI: "
    -- Right (_, v) -> V.forM_ v $ \ p ->
        -- putStrLn $ show (_midi p) ++ " earns " 
-- slicePiece :: (Foldable f, HasTime n) => Slicer n (TimeOf n) st s -> f n -> [s]
-- slicePiece slicer notes = groupsToSlices slicer $ onOffGroups notes

-- onOffGroups :: (Foldable f, HasTime n) => f n -> [[OnOff n (TimeOf n)]]
-- onOffGroups notes = L.groupBy eq $ L.sortOn onset $ foldl convert mempty notes
--  where
--   convert onoffs note =
--     Onset note (onset note) : Offset note (offset note) : onoffs
--   eq a b = onset a == onset b

-- notes = (Foldable f, HasTime n) => f n

sliceFromSalami :: 
  [SalamiNote] ->
  [[(Pitch SInterval, Music.RightTied)]]
sliceFromSalami salamiNotes = 
      mkSlice <$> filter (not . null) slices 
    where
      notes = noteFromSalami <$> salamiNotes
      slices = Music.slicePiece Music.tiedSlicer notes
      mkSlice notes = mkNote <$> notes
      mkNote (note,tie) = (Music.pitch note, Music.rightTie tie)

noteFromSalami :: SalamiNote ->  Music.Note SInterval (Ratio Int)  
noteFromSalami s = Music.Note sPitch onset offset 
  where
    midi' = midi s
    sPitch = sPitchFromMidi midi'
    (onset, offset) = (2,4)
    -- (onset, offset) = _onset_slice s
    
sPitchFromMidi :: Int -> SPitch
sPitchFromMidi midi = Music.toPitch $ spelled 2 3
