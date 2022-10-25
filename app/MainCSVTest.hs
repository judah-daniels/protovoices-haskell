{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- CSV Imports
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
-- Musicology imports
import qualified Data.Map.Strict as DM

import Data.Ratio
import qualified Data.Vector as V
import qualified Musicology.Core as Music
import qualified Musicology.Core.Slicing as Music
import qualified Musicology.MusicXML as MusicXML
import Musicology.Pitch
  ( Interval,
    MidiInterval,
    Notation (..),
    Pitch,
    SInterval,
    SPC,
    SPitch,
    pc,
  )
import Musicology.Pitch.Spelled as MT

type IntervalType = (Ratio Int, Ratio Int)

instance FromField Music.RightTied where
  parseField s = case s of
    "True" -> pure Music.Holds
    _ -> pure Music.Ends

instance FromField Bool where
  parseField s = case s of
    "True" -> pure True
    _ -> pure False

-- | Data Structure for parsing individual notes
data SalamiNote = SalamiNote
  { _segment_id :: !Int,
    _slice_id :: !Int, 
    _midi :: !Int,
    _tpc :: !Int,
    _tied :: !Music.RightTied
  }

-- | Data Structure for parsing chord labels
data ChordLabel = ChordLabel
  { _segment_id' :: !Int,
    _chord :: !String,
    _globalkey :: !String,
    _globalkey_is_minor :: !Bool
  }

instance FromNamedRecord SalamiNote where
  parseNamedRecord r =
    SalamiNote
      <$> r .: "segment_id"
      <*> r .: "slice_id"
      <*> r .: "midi"
      <*> r .: "tpc"
      <*> r .: "tied"

instance FromNamedRecord ChordLabel where
  parseNamedRecord r =
    ChordLabel
      <$> r .: "segment_id"
      <*> r .: "chord"
      <*> r .: "globalkey"
      <*> r .: "globalkey_is_minor"

main :: IO ()
main = do
  slices <- slicesFromFile "preprocessing/salamis.csv"
  chords <- chordsFromFile "preprocessing/chords.csv"

  print $ DM.lookup 0 slices  
  print $ DM.lookup 1 slices  
  print $ DM.lookup 2 slices  
  print $ DM.lookup 3 slices  

  print $ chords
  -- putStrLn $ head chords

  -- chordData <- BL.readFile "preprocessing/chords.csv"
  -- case decodeByName salamiData of
  --   Left err -> putStrLn err
  --   Right (_, v) -> print $ take 3 $ mkSlices (V.toList $ fmap noteFromSalami v)

type Chord = String

chordsFromFile :: FilePath -> IO [Chord]
chordsFromFile file = do
  txt <- BL.readFile file
  case decodeByName txt of
    Left err  -> pure [err]
    Right (_, v) -> do
      pure $ fmap parseChordLabel (V.toList v)       
  where
    parseChordLabel r = _chord r

-- | Datatype for a slice
type Slice = [(SPitch, Music.RightTied)]


slicesFromFile :: FilePath -> IO (DM.Map Int (DM.Map Int Slice))
-- slicesFromFile :: FilePath -> IO (DM.Map Int [(SPitch, Music.RightTied, Int)])
slicesFromFile file = do
  txt <- BL.readFile file
  case decodeByName txt of
    Left err  -> pure DM.empty 
    Right (_, v) -> do
      let notes  = fmap noteFromSalami (V.toList v)
          segmentedNotesList = segmentNotes <$> notes
          segmentedNotes = DM.fromAscListWith (++) segmentedNotesList
          segmentedSlices = DM.map mapFromSlices segmentedNotes -- Combine slices within segments
      pure segmentedSlices
    where
      segmentNotes (sPitch, tied, segmentId, sliceId) = (segmentId, [(sPitch,tied, sliceId)])
      mapFromSlices segment = DM.fromAscListWith (++) (segmentSlices <$> segment)   
      segmentSlices (sPitch, tied, sliceId) = (sliceId, [(sPitch, tied)])



noteFromSalami :: SalamiNote -> (SPitch, Music.RightTied, Int, Int)
noteFromSalami s = (sPitch, tied, segmentId, sliceId)
  where
    segmentId = _segment_id s
    sliceId = _slice_id s
    tied = _tied s
    oct = div (_midi s) 12
    tpc = _tpc s
    sPitch = spelledp tpc oct



-- mkSlices :: [Music.Note SInterval Float] -> [[(Music.Pitch , Music.RightTied)]]
-- mkSlices notes = slices
--   where
--     slices = Music.slicePiece Music.tiedSlicer notes 
-- sliceFromSalami ::
--   [SalamiNote] ->
--   [[(Pitch MidiInterval, Music.RightTied)]]
-- sliceFromSalami salamiNotes =
--   mkSlice <$> filter (not . null) slices
--   where
--     notes = noteFromSalami <$> salamiNotes
--     slices = Music.slicePiece Music.tiedSlicer notes
--     mkSlice notes = mkNote <$> notes
--     mkNote (note, tie) = (Music.pitch note, Music.rightTie tie)


-- noteFromSalami :: SalamiNote -> (Music.Note (Music.IntervalOf SPitch) Float, Music.RightTied)
-- noteFromSalami s = (Music.Note sPitch onset offset, tied')
--   where
--     oct = div (midi s) 12
--     tpc' = tpc s
--     tied' = tied s
--     sPitch = spelledp tpc' oct
--     onset = onset_slice_start s
--     offset = onset_slice_end s

-- (onset, offset) = _onset_slice s

-- 0 0 = C0  = 0
-- 0 1 = C1  = 24
-- 0 2 = C2  = 36
-- 0 3 = C3  = 48
-- 2 4 = D4  = 62
-- 2 5 = D6  = 86
-- 2 6 = D7
-- 1 6 = G6
--
