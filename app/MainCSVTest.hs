{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Common
import HeuristicParser
import HeuristicSearch
import PVGrammar
import Evaluator
import Data.Hashable

import Data.Csv
import Data.List.Split
import Data.Maybe
import Internal.MultiSet (MultiSet, fromList)

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import qualified Musicology.Core as Music

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
import PVGrammar.Parse (protoVoiceEvaluator)

import Control.Monad.Except
  ( ExceptT
  , MonadError (throwError), runExceptT
  )
import Control.Monad.IO.Class
  ( MonadIO
  )

import System.Random (initStdGen)
import System.Random.Stateful
  ( StatefulGen
  , newIOGenM
  , uniformRM
  , randomRIO
  )

import Control.Monad.Trans.Class (lift)
import Evaluator 
import HeuristicParser (printPathFromState)

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
    -- _globalkey_is_minor :: !Bool
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
      <*> r .: "chordtype"
      <*> r .: "rootoffset"
      <*> r .: "globalkey"
      -- <*> r .: "globalkey_is_minor"

main :: IO ()
main = do
  -- slices <- slicesFromFile' "preprocessing/salamis.csv"
  chords <- chordsFromFile "preprocessing/chords.csv"
  params <- loadParams "preprocessing/dcml_params.json"

  -- let initialState = SSFrozen $ pathFromSlices protoVoiceEvaluator slices

  -- doHeuristicSearch protoVoiceEvaluator initialState chords
  -- runHeuristicSearch protoVoiceEvaluator slices chords

  pure ()
  -- let path = pathFromSlices protoVoiceEvaluator slices  

type InputSlice ns = ([(ns, Music.RightTied)], Bool)

runHeuristicSearch 
  :: ( Music.HasPitch ns
     , Eq (Music.IntervalOf ns)
     , Data.Hashable.Hashable ns
     , Ord ns
     , Show ns
     , Music.Notation ns) 
  => Eval (Edges ns) [Edge ns] (Notes ns) [ns] (PVLeftmost ns)
  -> [InputSlice ns]
  -> [ChordLabel]
  -> IO (Path (Edges ns) (Notes ns))
runHeuristicSearch eval inputSlices chordLabels = do
  -- putStrLn "\nPlotting Derivation: "
  -- plotDeriv p ("output198" <> ".tex") ops

  pure p
    where
      initialState = SSFrozen $ pathFromSlices eval inputSlices

      finalState = fromMaybe initialState (heuristicSearch initialState getNeighboringStates goalTest heuristic printPathFromState)

      ops = getOpsFromState finalState
      p = fromMaybe undefined $ getPathFromState finalState

      getNeighboringStates = exploreStates eval

      -- The goal is to find a state with a slice for each chord label.
      goalTest (SSOpen p _) = pathLen p - 1 == length chordLabels
      goalTest _ = False

      -- Where the magic happens!
      heuristic x = 0

doHeuristicSearch 
  :: forall ns ns' es es' 
  .  (Notation ns,Show ns) 
  => Eval (Edges ns) [Edge ns] (Notes ns) [ns] (PVLeftmost ns)
  -> SearchState (Edges ns) [Edge ns] (Notes ns) (PVLeftmost ns)
  -> [ChordLabel]
  -> IO ()
doHeuristicSearch eval initialState chords  = do 

  -- let eval = protoVoiceEvaluator -- :: Eval (Edges SPC) [Edge SPitch] (Notes SPitch) [SPC] (PVLeftmost SPC)

  let getNeighboringStates = exploreStates eval

  -- Do the search!!!
  putStrLn "Doing Search"
  let finalState = fromMaybe initialState (heuristicSearch initialState getNeighboringStates goalTest heuristic printPathFromState)
  
  let ops = getOpsFromState finalState
  let p = getPathFromState finalState

  putStrLn "Derivation complete: "

  putStrLn $ "\nFinal Path: " <> show p

  hpData <- loadParams "preprocessing/dcml_params.json"
  -- let res = evalPath p chords hpData
  -- print ops

  putStrLn $ "\nEvaluation score: " -- <> show res
    where
      goalTest s = case s of  
        SSSemiOpen {} -> False 
        SSFrozen {} -> False
        SSOpen p _ -> oneChordPerSegment p
          where
            oneChordPerSegment (PathEnd _ ) = True
            oneChordPerSegment (Path tl _ rst) = tBoundary tl && oneChordPerSegment rst

      -- Where the magic happens!
      heuristic x = 0 

pathFromSlices 
  :: forall ns 
  .  Eval (Edges ns) [Edge ns] (Notes ns) [ns] (PVLeftmost ns)
  -> [ ([(ns, Music.RightTied)], Bool)] 
  -> Path (Maybe [Edge ns], Bool) (Slice (Notes ns)) 
pathFromSlices eval = reversePath . mkPath (Nothing, False) 
  where
    mkPath 
      :: (Maybe [Edge ns], Bool) 
      -> [([(ns, Music.RightTied)], Bool)] 
      -> Path (Maybe [Edge ns], Bool) (Slice (Notes ns)) 

    mkPath eLeft ((slice,boundary):rst) = Path eLeft (Slice $ evalSlice' (map fst slice)) $ mkPath (Just $ getTiedEdges slice, boundary) rst
    mkPath eLeft [] = PathEnd (Nothing, False)
    
    evalSlice' :: [ns] -> Notes ns 
    evalSlice' = evalSlice eval

    getTiedEdges :: [(ns, Music.RightTied)] -> [Edge ns]
    getTiedEdges  = mapMaybe mkTiedEdge 
      where   
        mkTiedEdge :: (ns, Music.RightTied) -> Maybe (Edge ns)
        mkTiedEdge (_, Music.Ends) = Nothing
        mkTiedEdge (pitch, Music.Holds) = Just (Inner pitch, Inner pitch)
    
    
printSlicesWithLabels' :: [(Slice', Bool)] -> [Chord] -> IO ()
printSlicesWithLabels' slices chords = do
  mapM_ (printLine chords slices) [0 .. 7]
  where
    printSlice :: Slice' -> IO ()
    printSlice slice = do
      print slice

    printSegment :: [(Slice', Bool)] -> Int -> IO ()
    printSegment segment i = do
      f segment i
      where 
        f [] _ = pure ()
        f _ (-1) = pure ()

        f ((s,b):sx) i = do
          if b then 
            if i == 1 then 
              do 
                print s
                f sx (i-1)
            else 
              f sx (i-1)
          else
            if i == 0 then 
              do 
                print s 
                f sx i
            else 
              f sx i

    printLine :: [Chord] -> [(Slice',Bool)] -> Int -> IO ()
    printLine chords slices i = do
      putStrLn $ chords !! i <> ": "
      printSegment slices (i+1)  

-- | Prints out the chords and corresponding slices for debugging.
printSlicesWithLabels :: [[Slice']] -> [Chord] -> IO ()
printSlicesWithLabels slices chords = do
  mapM_ (printLine chords slices) [0 .. 10]
  where
    printSlice :: Slice' -> IO ()
    printSlice slice = do
      print slice

    printSegment :: [Slice'] -> IO ()
    printSegment segment = do
      mapM_ printSlice segment
    -- mapM_ (show) [0 .. (DM.size segment)]
    printLine :: [Chord] -> [[Slice']] -> Int -> IO ()
    printLine chords slices i = do
      putStrLn $ chords !! i <> ": "
      printSegment (slices !! i)

-- | Data type for a chord Label. TODO specialise this datatype.
type Chord = String

-- | Loads chord annotations from filepath
chordsFromFile :: FilePath -> IO [Evaluator.ChordLabel]
chordsFromFile file = do
  txt <- BL.readFile file
  case decodeByName txt of
    Left err -> pure []
    Right (_, v) -> do
      pure $ fmap parseChordLabel (V.toList v)
  where
    parseChordLabel r = Evaluator.ChordLabel (_chordtype r) (Music.sic $ _rootoffset r) (fromMaybe undefined (readNotation $ _globalkey r))

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
          segmentedNotes :: [[(SPitch, Music.RightTied, Bool, Bool)]]
          segmentedNotes = splitWhen (\(_, _, newSeg, _) -> newSeg) notes
          segmentedNotes' = (map . map) (\(s, t, _, n) -> (s, t, n)) segmentedNotes
          -- segmentedSlices :: [(Slice, Bool)]
          segmentedSlices = map ((map . map) (\(s, t, _) -> (s, t)) . splitWhen (\(_, _, newSlice) -> newSlice)) segmentedNotes'
          b = output segmentedSlices
      pure b
          where
            output :: [[[(SPitch,Music.RightTied)]]] -> [(Slice', Bool)]
            -- f :: [[a]] -> (a->b) -> [b]
            output = concatMap addBoundaries 

            -- Assign boundary marker to first slice of each segment
            addBoundaries :: [[(SPitch, Music.RightTied)]] -> [(Slice', Bool)]
            addBoundaries [] = []
            addBoundaries (s:sx) = (s, True):map (, False) sx

-- Parse a salami note as output from the python salalmis package.
noteFromSalami :: SalamiNote -> (SPitch, Music.RightTied, Bool, Bool)
noteFromSalami s = (sPitch, tied, newSegment, newSlice)
  where
    newSegment = _new_segment s
    newSlice = _new_slice s
    tied = _tied s
    sPitch = Data.Maybe.fromMaybe undefined (readNotation $ _pitch s) -- Refactor to deal with maybe
