{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List.Split
-- Musicology imports
import qualified Data.Map.Strict as DM
import Data.Maybe
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
  { _new_segment :: !Bool,
    _new_slice :: !Bool,
    _pitch :: !String,
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
      <$> r .: "new_segment"
      <*> r .: "new_slice"
      <*> r .: "pitch"
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

  printSlicesWithLabels slices chords

-- | Prints out the chords and corresponding slices for debugging.
printSlicesWithLabels :: [[Slice]] -> [Chord] -> IO ()
printSlicesWithLabels slices chords = do
  mapM_ (printLine chords slices) [0 .. 10]
  where
    printSlice :: Slice -> IO ()
    printSlice slice = do
      print slice

    printSegment :: [Slice] -> IO ()
    printSegment segment = do
      mapM_ printSlice segment
    -- mapM_ (show) [0 .. (DM.size segment)]
    printLine :: [Chord] -> [[Slice]] -> Int -> IO ()
    printLine chords slices i = do
      putStrLn $ chords !! i <> ": "
      printSegment (slices !! i)

-- | Data type for a chord Label. TODO specialise this datatype.
type Chord = String

-- | Loads chord annotations from filepath
chordsFromFile :: FilePath -> IO [Chord]
chordsFromFile file = do
  txt <- BL.readFile file
  case decodeByName txt of
    Left err -> pure [err]
    Right (_, v) -> do
      pure $ fmap parseChordLabel (V.toList v)
  where
    parseChordLabel r = _chord r

-- | Datatype for a slice
type Slice = [(SPitch, Music.RightTied)]

-- [(SPitch, RightTied, SegmentID, sliceId)] -> []

-- | Loads slices from filepath
-- slicesFromFile :: FilePath -> IO (DM.Map Int (DM.Map Int Slice))
slicesFromFile :: FilePath -> IO [[Slice]]
slicesFromFile file = do
  txt <- BL.readFile file
  case decodeByName txt of
    Left err -> pure []
    Right (_, v) -> do
      let notes = fmap noteFromSalami (V.toList v)
          segmentedNotes :: [[(SPitch, Music.RightTied, Bool, Bool)]]
          segmentedNotes = splitWhen (\(_, _, newSeg, _) -> newSeg) notes
          segmentedNotes' = (map . map) (\(s, t, _, n) -> (s, t, n)) segmentedNotes
          segmentedSlices :: [[Slice]]
          segmentedSlices = map (((map . map) (\(s, t, _) -> (s, t))) . splitWhen (\(_, _, newSlice) -> newSlice)) segmentedNotes'
      pure segmentedSlices

-- where

-- segmentNotes (sPitch, tied, segmentId, sliceId) = (segmentId, [(sPitch, tied, sliceId)])

--         segmentedNotesList = segmentNotes <$> notes
--         segmentedNotes = DM.fromAscListWith (++) segmentedNotesList
--         segmentedSlices = DM.map mapFromSlices segmentedNotes -- Combine slices within segments
--     pure segmentedSlices
-- where
--   segmentNotes (sPitch, tied, segmentId, sliceId) = (segmentId, [(sPitch, tied, sliceId)])
--   mapFromSlices segment = DM.fromAscListWith (++) (segmentSlices <$> segment)
--   segmentSlices (sPitch, tied, sliceId) = (sliceId, [(sPitch, tied)])

-- Parse a salami note as output from the python salalmis package.
noteFromSalami :: SalamiNote -> (SPitch, Music.RightTied, Bool, Bool)
noteFromSalami s = (sPitch, tied, newSegment, newSlice)
  where
    newSegment = _new_segment s
    newSlice = _new_slice s
    tied = _tied s
    sPitch = Data.Maybe.fromMaybe undefined (readNotation $ _pitch s) -- Refactor to deal with maybe
