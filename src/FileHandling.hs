{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | This module contains functionality for reading chord and slice data from csv files, as output by 'preprocess.py'.
     This also contains functionality for writing the results of the parses to json files, to be read by 'results.py'
-}
module FileHandling
  ( InputSlice
  , JsonResult (..)
  , PieceResults (..)
  , pathFromSlices
  , concatResults
  , slicesFromFile'
  , chordsFromFile
  , writeJSONToFile
  , writeResultsToJSON
  , nullResultToJSON
  , splitSlicesIntoSegments
  ) where

import Common hiding (split)
import Control.Logging qualified as Log
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Control.Monad.State (evalState)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson (JSONPath, (.=))
import Data.Aeson qualified as A
import Data.Aeson.Encoding
import Data.Aeson.Key qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.Csv (FromField, FromNamedRecord, FromRecord, decodeByName, parseField, parseNamedRecord, parseRecord, (.:))
import Data.Hashable
import Data.List.Split
import Data.Map.Strict qualified as M
import Data.Maybe
  ( catMaybes
  , fromJust
  , fromMaybe
  , isNothing
  , mapMaybe
  , maybeToList
  )
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace
import Display
import GHC.Generics
import Parser.HeuristicParser
import Heuristics
import Internal.MultiSet qualified as MS
import Language.Haskell.DoNotation
import Musicology.Core
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled
import Harmony.ChordLabel
import Harmony
import Harmony.Params
import PVGrammar hiding
  ( slicesFromFile
  )
import PVGrammar.Generate
import PVGrammar.Parse
import Prelude hiding
  ( Monad (..)
  , lift
  , pure
  )

import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import Text.Read (readMaybe)

-- | Slice type for reading from csv
type InputSlice ns =
  ( [ ( ns
      , -- \^ Note
        Music.RightTied
      )
    ]
  , -- \^ Marks whether or not this note is tied to the next

    Bool
  )

-- \^ Marks if this slice is the start of a new segment

-- -- ---------------------------------- -------------------------------- --------------------------------
-- Reading Data from Files

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
  { _new_segment :: !Bool
  , _new_slice :: !Bool
  , _pitch :: !String
  , _tied :: !Music.RightTied
  }

-- | Data Structure for parsing chord labels
data ChordLabel' = ChordLabel'
  { _segment_id' :: !Int
  , _chordtype :: !String
  , _rootnote :: !String
  , _globalkey :: !String
  }

instance FromNamedRecord SalamiNote where
  parseNamedRecord r =
    SalamiNote
      <$> r .: "new_segment"
      <*> r .: "new_slice"
      <*> r .: "pitch"
      <*> r .: "tied"

instance FromNamedRecord ChordLabel' where
  parseNamedRecord r =
    ChordLabel'
      <$> r .: "segment_id"
      <*> r .: "chord_type"
      <*> r .: "rootnote"
      <*> r .: "globalkey"

-- | Data type for a chord type. TODO specialise this datatype.
type Chord = String

-- | Intermediate datatype for a slice, once boundaries have been parsed.
type Slice' = [(SPitch, Music.RightTied)]

-- | Loads chord annotations from filepath
chordsFromFile :: FilePath -> IO [ChordLabel]
chordsFromFile file = do
  txt <- BL.readFile file
  case decodeByName txt of
    Left err -> do
      print err
      pure []
    Right (_, v) -> do
      pure $ fmap parseChordLabel (V.toList v)
 where
  parseChordLabel :: ChordLabel' -> ChordLabel
  parseChordLabel r = ChordLabel cType rootNote'
   where
    -- (Music.sic $ _rootoffset r) (fromMaybe undefined (Music.readNotation $ _globalkey r))

    cType :: ChordType
    cType = case readMaybe $ _chordtype r of
              Nothing -> trace "Cant read chord type" Major -- BUGBUG BUG BGU TODO FIX
              Just c -> c

    rootNote' :: SPC
    rootNote' = case Music.readNotation $ _rootnote r of
      Just note -> note
      Nothing -> Log.errorL (T.concat ["Can't Read notation of rootnote from ", T.pack file, " key: ", T.pack $ _globalkey r])
    -- globalKey' :: SPC
    -- globalKey' = case Music.readNotation $ _globalkey r of
    --   Just note -> note
    --   Nothing -> Log.errorL (T.concat ["Can't Read notation of global key from ", T.pack file, " key: ", T.pack $ _globalkey r])

-- | Loads slices from filepath
slicesFromFile' :: FilePath -> IO [(Slice', Bool)]
slicesFromFile' file = do
  txt <- BL.readFile file
  case decodeByName txt of
    Left err -> pure []
    Right (_, v) -> do
      let notes = fmap noteFromSalami (V.toList v)
      -- notes :: [(SPitch, Music.RightTied, NewSegment, NewSlice])
      let slices =
            let
              splitNotes = split (keepDelimsL $ whenElt (\(_, _, _, newSlice) -> newSlice)) notes
             in
              (map . map) (\(s, t, newSeg, _) -> (s, t, newSeg)) splitNotes

      -- let segmentedSlices = map addBoundary slices
      let segmentedSlices = case map addBoundary slices of
            (x, _) : rst -> (x, True) : rst
            [] -> []

      pure segmentedSlices
     where
      addBoundary :: [(SPitch, Music.RightTied, Bool)] -> (Slice', Bool)
      addBoundary [] = undefined
      addBoundary slc@((p, t, True) : rst) = (dropLastOf3 <$> slc, True)
      addBoundary slc@((p, t, False) : rst) = (dropLastOf3 <$> slc, False)

      dropLastOf3 (a, b, c) = (a, b)

      output :: [[[(SPitch, Music.RightTied)]]] -> [(Slice', Bool)]
      -- f :: [[a]] -> (a->b) -> [b]
      output = concatMap addBoundaries

      splitIntoSlices :: [(SPitch, Music.RightTied, Bool)] -> [[(SPitch, Music.RightTied)]]
      splitIntoSlices slices = (map . map) (\(s, t, _) -> (s, t)) $ split (keepDelimsR $ whenElt (\(_, _, newSlice) -> newSlice)) slices

      -- Assign boundary marker to first slice of each segment
      addBoundaries :: [[(SPitch, Music.RightTied)]] -> [(Slice', Bool)]
      addBoundaries [] = []
      addBoundaries (s : sx) = (s, True) : map (,False) sx

-- | Parse a salami note as output from the DCML dimcat package.
noteFromSalami :: SalamiNote -> (SPitch, Music.RightTied, Bool, Bool)
noteFromSalami s = (sPitch, tied, newSegment, newSlice)
 where
  newSegment = _new_segment s
  newSlice = _new_slice s
  tied = _tied s
  sPitch = Data.Maybe.fromJust (Music.readNotation $ _pitch s) -- Refactor to deal with maybe

splitSlicesIntoSegments
  :: forall ns o
   . Eval (Edges ns) [Edge ns] (Notes ns) [ns] o
  -> SliceWrapper (Notes ns)
  -> [InputSlice ns]
  -> [[InputSlice ns]]
splitSlicesIntoSegments eval wrap = split (dropInitBlank . keepDelimsL $ whenElt snd)

-- | Create a 'Path' given a list of slices
pathFromSlices
  :: forall ns o
   . Eval (Edges ns) [Edge ns] (Notes ns) [ns] o
  -> SliceWrapper (Notes ns)
  -> [InputSlice ns]
  -> Path (Maybe [Edge ns], Bool) (SliceWrapped (Notes ns))
pathFromSlices eval wrap = reversePath . mkPath False Nothing
 where
  mkPath
    :: Bool
    -> Maybe [Edge ns]
    -> [InputSlice ns]
    -> Path (Maybe [Edge ns], Bool) (SliceWrapped (Notes ns))

  mkPath tie eLeft [] = PathEnd (eLeft, True)
  mkPath tie eLeft ((slice, boundary) : rst) =
    Path (eLeft, boundary) nextSlice $
      mkPath False (Just $ getTiedEdges slice) rst
   where
    nextSlice = wrapSlice wrap $ evalSlice eval (fst <$> slice)

  getTiedEdges :: [(ns, Music.RightTied)] -> [Edge ns]
  getTiedEdges = mapMaybe mkTiedEdge
   where
    mkTiedEdge :: (ns, Music.RightTied) -> Maybe (Edge ns)
    mkTiedEdge (p, Music.Holds) = Just (Inner p, Inner p)
    mkTiedEdge _ = Nothing


-- Writing parse results to JSON
-- ===========

-- | Alias for the accuracy of the predicted labels compared to the ground truth
type Accuracy = Double
-- | Alias for the loglikelihood of the entire piece given the chordlabels, according to the probabilistic model of harmony
type LogLikelihood = Double
-- | Alias for how long the algorithm ran for, measured in TODO
type Time = Double

-- | Stores raw info for the json object of a specific run
data JsonResult = JsonResult
  { jrSlices :: [Notes SPitch]
  , jrLabels :: [ChordLabel]
  , jrDeriv :: Maybe (PVAnalysis SPitch)
  , jrAccuracy :: Accuracy
  , jrLogLikelihood :: LogLikelihood
  , jrAlgoName :: String
  , jrRunTime :: Time
  , jrReRuns :: Int
  , jrId :: Int
  , jrSegmentTimes :: Maybe [Time]
  }

-- | Stores all the info required to output the results from running an algorithm n times on a single piece
data PieceResults = PieceResults
  { prExpId :: String
  , prAlgoName :: String
  , prCorpus :: String
  , prPiece :: String
  , prGroundTruth :: [ChordLabel]
  , prJsonResultObjects :: [A.Value]
  }

-- | Coverts the results of a parsing algorithm to a JSON value
writeResultsToJSON
  :: JsonResult
  -> A.Value
writeResultsToJSON res =
-- (JsonResult slices chords derivation accuracy likelihood name runTime reruns id segmentTimes) =
  A.object
    [
     "slices" .= ((\(Notes x) -> show <$> MS.toList x) <$> jrSlices res)
    , "chordLabels" .= (show <$> jrLabels res)
    , "accuracy" .= jrAccuracy res
    , "likelihood" .= jrLogLikelihood res
    , "runTime" .= jrRunTime res
    , "reruns" .= jrReRuns res
    , "iteration" .= jrId res
    , "segmentTimes" .= jrSegmentTimes res
    ]

-- | Concatenates all results for an algo on a given piece into an object, inlucuding the piece and corpus in the JSON value.
concatResults :: PieceResults -> A.Value
concatResults (PieceResults expId algoName corpus piece trueLabels results) =
  A.object
    ["id" .= A.fromString expId
    , "algorithm" .= A.fromString algoName
    , "corpus" .= A.fromString corpus
    , "piece" .= A.fromString piece
    , "results" .= results
    , "groundTruth" .= (show <$> trueLabels)]

-- | Write JSON value to the given file
writeJSONToFile :: A.ToJSON a => FilePath -> a -> IO ()
writeJSONToFile filePath v = do
  createDirectoryIfMissing True $ takeDirectory filePath
  BL.writeFile filePath (A.encode v)

-- | Used when the algorithm fails. Contains NANs in all fields.
nullResultToJSON :: Show a => a -> A.Value
nullResultToJSON a =
    A.object
      [
      -- "algorithm" .= A.fromString (show a)
       "slices" .= (Nothing :: Maybe [String])
      , "chordLabels" .= (Nothing :: Maybe [String])
      , "accuracy" .= (Nothing :: Maybe Float)
      -- , "ops" .= (Nothing :: Maybe )
      , "likelihood" .= (Nothing :: Maybe Float)
      , "reRuns" .= (Nothing :: Maybe Int)
      , "runTime" .= (Nothing :: Maybe Time)
      , "iteration" .= (Nothing :: Maybe Int)
    , "segmentTimes" .= (Nothing :: Maybe Time)
      ]
