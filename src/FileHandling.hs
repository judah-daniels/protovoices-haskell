
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileHandling 
  ( InputSlice
  , pathFromSlices
  , slicesFromFile'
  , chordsFromFile
  , writeMapToJson
  , writeJSONToFile
  , writeResultsToJSON
  , splitSlicesIntoSegments
  ) where

import Musicology.Core

-- LOGGING
import qualified Data.Text as T
import Control.Logging qualified as Log

import Debug.Trace
import Common hiding (split)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Control.Monad.Except (ExceptT,runExceptT, lift, throwError)
import Data.Csv ((.:), parseField, decodeByName, parseNamedRecord, parseRecord, FromField, FromNamedRecord, FromRecord)
import Data.List.Split
import Data.Hashable
import Data.Maybe
  ( catMaybes,
    isNothing,
    fromMaybe,
    fromJust,
    mapMaybe,
    maybeToList,
  )
import Data.Vector qualified as V
import Display
import RandomChoiceSearch
import RandomSampleParser
import HeuristicSearch
import PBHModel
import Language.Haskell.DoNotation
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled
import Heuristics
import PVGrammar hiding
  ( slicesFromFile,
  )
import PVGrammar.Generate
import PVGrammar.Parse
import Prelude hiding
  ( Monad (..),
    lift,
    pure,
  )
import Control.Monad.State (evalState)
import Control.Monad.Trans.Except (throwE)
import qualified Internal.MultiSet as MS
import HeuristicParser 
import Data.Aeson (JSONPath, (.=))
import Data.Aeson qualified as A
import Data.Aeson.Encoding 
import GHC.Generics



-- ([(Note, Tied?)], Start of new segment)
type InputSlice ns = ([(ns, Music.RightTied)], Bool)
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
  { _new_segment :: !Bool,
    _new_slice :: !Bool,
    _pitch :: !String,
    _tied :: !Music.RightTied
  }

-- | Data Structure for parsing chord labels
data ChordLabel' = ChordLabel'
  { _segment_id' :: !Int,
    _chordtype :: !String,
    _rootoffset :: !Int,
    _globalkey :: !String
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
      <*> r .: "rootoffset"
      <*> r .: "globalkey"

-- | Data type for a chord Label. TODO specialise this datatype.
type Chord = String

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
    parseChordLabel r = ChordLabel cType ((Music.+^) globalKey'  rootOffset')
    -- (Music.sic $ _rootoffset r) (fromMaybe undefined (Music.readNotation $ _globalkey r))
      where 
        cType :: String
        cType = _chordtype r
        rootOffset' :: SIC
        rootOffset' = Music.sic $ _rootoffset r
        globalKey' :: SPC
        globalKey' = case Music.readNotation $ _globalkey r of 
                       Just note -> note
                       Nothing -> Log.errorL  ( T.concat ["Can't Read notation of global key from " , T.pack file, " key: ", T.pack $ _globalkey r ])

-- | Datatype for a slice
type Slice' = [(SPitch, Music.RightTied)]

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
             splitNotes = split (keepDelimsL $ whenElt (\(_,_,_,newSlice) -> newSlice)) notes 
            in (map . map) (\(s,t,newSeg,_) -> (s,t,newSeg)) splitNotes

      -- let segmentedSlices = map addBoundary slices
      let segmentedSlices = case map addBoundary slices of 
                              (x, _):rst -> (x, True):rst
                              [] -> []
      
      pure segmentedSlices
      where
        addBoundary :: [(SPitch, Music.RightTied, Bool)] -> (Slice', Bool)
        addBoundary [] = undefined 
        addBoundary slc@((p, t, True):rst) = (dropLastOf3 <$> slc, True)
        addBoundary slc@((p, t, False):rst) = (dropLastOf3 <$> slc, False)


        dropLastOf3 (a,b,c) = (a,b)

        output :: [[[(SPitch, Music.RightTied)]]] -> [(Slice', Bool)]
        -- f :: [[a]] -> (a->b) -> [b]
        output = concatMap addBoundaries

        splitIntoSlices :: [(SPitch, Music.RightTied, Bool)] -> [[(SPitch, Music.RightTied)]]
        splitIntoSlices slices = (map . map) (\(s,t,_) -> (s,t)) $ split (keepDelimsR $ whenElt (\(_,_,newSlice) -> newSlice)) slices

        -- Assign boundary marker to first slice of each segment
        addBoundaries :: [[(SPitch, Music.RightTied)]] -> [(Slice', Bool)]
        addBoundaries [] = []
        addBoundaries (s : sx) = (s, True) : map (,False) sx

-- Parse a salami note as output from the python salalmis package.
noteFromSalami :: SalamiNote -> (SPitch, Music.RightTied, Bool, Bool)
noteFromSalami s = (sPitch, tied, newSegment, newSlice)
  where
    newSegment = _new_segment s
    newSlice = _new_slice s
    tied = _tied s
    sPitch = Data.Maybe.fromJust (Music.readNotation $ _pitch s) -- Refactor to deal with maybe

splitSlicesIntoSegments ::
  forall ns o.
  Eval (Edges ns) [Edge ns] (Notes ns) [ns] o ->
  SliceWrapper (Notes ns) ->
  [InputSlice ns] ->
  [[InputSlice ns]]
splitSlicesIntoSegments eval wrap = split (dropInitBlank . keepDelimsL $ whenElt snd) 
-- type InputSlice ns = ([(ns, Music.RightTied)], Bool)
  --
  --
  -- reversePath . mkPath False Nothing
  -- where
  --   mkPath ::
  --     Bool ->
  --     Maybe [Edge ns] ->
  --     [InputSlice ns] ->
  --     Path (Maybe [Edge ns], Bool) (SliceWrapped (Notes ns))
  --
  --   mkPath tie eLeft [] = PathEnd (eLeft, True)
  --
  --   mkPath tie eLeft ((slice, boundary) : rst) =
  --     Path (eLeft, boundary) nextSlice $
  --       mkPath False (Just $ getTiedEdges slice) rst
  --         where 
  --           nextSlice = wrapSlice wrap $ evalSlice eval (fst <$> slice)
  --
  --
  --   getTiedEdges :: [(ns, Music.RightTied)] -> [Edge ns]
  --   getTiedEdges = mapMaybe mkTiedEdge
  --     where
  --       mkTiedEdge :: (ns, Music.RightTied) -> Maybe (Edge ns)k
  --       mkTiedEdge (p, Music.Holds) = Just (Inner p, Inner p)
  --       mkTiedEdge _ = Nothing
  --

pathFromSlices ::
  forall ns o.
  Eval (Edges ns) [Edge ns] (Notes ns) [ns] o ->
  SliceWrapper (Notes ns) ->
  [InputSlice ns] ->
  Path (Maybe [Edge ns], Bool) (SliceWrapped (Notes ns))
pathFromSlices eval wrap = reversePath . mkPath False Nothing
  where
    mkPath ::
      Bool ->
      Maybe [Edge ns] ->
      [InputSlice ns] ->
      Path (Maybe [Edge ns], Bool) (SliceWrapped (Notes ns))

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

---- Writing Data to JSONPath

writeMapToJson :: [(String, Double)] -> FilePath -> IO ()
writeMapToJson dict fileName = do 
  -- fileHandle <- openFile fileName
  let json = A.toJSON (M.fromList dict)
  Log.debug $ T.pack . show $ json
  BL.writeFile fileName (A.encode json)
  -- closeFile fileHandle


-- data ParseResults = ParseResults 
--   { _slices :: [Notes SPitch] 
--   , _chords :: [ChordLabel] 
--   , _path :: Maybe (Path (Notes SPitch) (Edges SPitch)) 
--   , _labelAccuracy :: Double 
--   , _labelLikelihood :: Double
--   } deriving (Show, Generic, A.ToJSON , A.FromJSON)

writeResultsToJSON 
  :: [Notes SPitch]
  -> [ChordLabel]
  -> Maybe (Path (Edges SPitch) (Notes SPitch))
  -> Double
  -> Double
  -> A.Value
writeResultsToJSON slices chords pathMaybe accuracy likelihood 
  = A.object  
    [ "Slices" .= ((\(Notes x) -> show <$> MS.toList x) <$> slices)
    , "ChordLabels" .= (show <$> chords)
    -- , "Path" .= pathMaybe
    , "Accuracy" .= accuracy
    , "Likelihood" .= likelihood]



writeJSONToFile :: A.ToJSON a =>  FilePath -> a -> IO ()
writeJSONToFile filePath v = BL.writeFile filePath (A.encode v)

-- dict fileName = do 
  -- fileHandle <- openFile fileName
  -- let json = A.toJSON (M.fromList dict)
  -- Log.debug $ T.pack . show $ json
  -- closeFile fileHandle


