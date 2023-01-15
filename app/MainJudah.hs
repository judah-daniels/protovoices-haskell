{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE RebindableSyntax #-}

module Main where

import Debug.Trace
import Common
import Data.ByteString.Lazy qualified as BL
import Control.Monad.Except (ExceptT,runExceptT, lift, throwError)
import Data.Csv
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
import Evaluator
import HeuristicParser
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

type InputSlice ns = ([(ns, Music.RightTied)], Bool)

main :: IO ()
main = do
  -- slices <- slicesFromFile' "preprocessing/salamis.csv"
  slices <- slicesFromFile' "preprocessing/salamisShortest.csv"
  -- chords <- chordsFromFile "preprocessing/chords.csv"
  chords <- chordsFromFile "preprocessing/chordsShortest.csv"
  params <- loadParams "preprocessing/dcml_params.json"
  -- finalPath <- runHeuristicSearch protoVoiceEvaluator slices321sus chords321sus
  (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slices chords

  let res = evalPath finalPath chords params
  -- let res = evalPath finalPath chords321sus hpData

  putStrLn $ "\nFinal Path: " <> show finalPath
  putStrLn $ "\nEvaluation score: " <> show res


---------------------------------- -------------------------------- -------------------------------- |
-- INPUTS FOR TESTING
--
-- The musical surface from Figure 4 as a sequence of slices and transitions.
-- Can be used as an input for parsing.
path321sus =
  Path [e nat 4, c nat 4] [(Inner $ c nat 4, Inner $ c nat 4)] $
    Path [d nat 4, c nat 4] [(Inner $ d nat 4, Inner $ d nat 4)] $
      Path [d nat 4, b nat 4] [] $
        PathEnd [c nat 4]

testInput :: [InputSlice SPC]
testInput =
  [ ([(e' nat, Music.Holds), (c' nat, Music.Ends)], False),
    ([(e' nat, Music.Holds)], True),
    ([(e' nat, Music.Ends)], False)
  ]

slices321sus :: [InputSlice SPitch]
slices321sus =
  [ ([(e nat 4, Music.Ends), (c nat 4, Music.Holds)], True),
    ([(d nat 4, Music.Holds), (c nat 4, Music.Ends)], False),
    ([(d nat 4, Music.Holds), (b nat 4, Music.Ends)], True),
    ([(c nat 4, Music.Ends)], True)
  ]

chords321sus :: [ChordLabel]
chords321sus =
  [ ChordLabel "M" (sic 0) (c' nat),
    ChordLabel "M" (sic 1) (c' nat),
    ChordLabel "M" (sic 0) (c' nat)
  ]

---------------------------------- -------------------------------- --------------------------------
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
    Left err -> pure []
    Right (_, v) -> do
      pure $ fmap parseChordLabel (V.toList v)
  where
    parseChordLabel r = ChordLabel (_chordtype r) (Music.sic $ _rootoffset r) (fromMaybe undefined (Music.readNotation $ _globalkey r))

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
        output :: [[[(SPitch, Music.RightTied)]]] -> [(Slice', Bool)]
        -- f :: [[a]] -> (a->b) -> [b]
        output = concatMap addBoundaries

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
    sPitch = Data.Maybe.fromMaybe (c nat 0) (Music.readNotation $ _pitch s) -- Refactor to deal with maybe

pathFromSlices ::
  forall ns o.
  Eval (Edges ns) [Edge ns] (Notes ns) [ns] o ->
  [InputSlice ns] ->
  Path (Maybe [Edge ns], Bool) (Slice (Notes ns))
pathFromSlices eval = reversePath . mkPath Nothing
  where
    mkPath ::
      Maybe [Edge ns] ->
      [InputSlice ns] ->
      Path (Maybe [Edge ns], Bool) (Slice (Notes ns))

    mkPath eLeft ((slice, boundary) : rst) =
      Path (eLeft, boundary) (Slice $ evalSlice eval (fst <$> slice)) $
        mkPath (Just $ getTiedEdges slice) rst
    mkPath eLeft [] = PathEnd (Nothing, True)

    getTiedEdges :: [(ns, Music.RightTied)] -> [Edge ns]
    getTiedEdges = mapMaybe mkTiedEdge
      where
        mkTiedEdge :: (ns, Music.RightTied) -> Maybe (Edge ns)
        mkTiedEdge (pitch, Music.Holds) = Just (Inner pitch, Inner pitch)
        mkTiedEdge _ = Nothing


runHeuristicSearch ::
  ( Music.HasPitch ns,
    Eq (Music.IntervalOf ns),
    Data.Hashable.Hashable ns,
    Ord ns,
    Show ns,
    Music.Notation ns
  ) 
  => HarmonicProfileData 
  -> Eval (Edges ns) [Edge ns] (Notes ns) [ns] (PVLeftmost ns) 
  -> (SearchState (Edges ns) [Edge ns] (Notes ns) (PVLeftmost ns) -> ExceptT String IO Double)
  -> [InputSlice ns] 
  -> [ChordLabel] 
  -> IO (Path (Edges ns) (Notes ns), [PVLeftmost ns])
runHeuristicSearch params eval heuristic inputSlices chordLabels = do
  let initialState = SSFrozen $ pathFromSlices eval inputSlices
  res <- runExceptT (heuristicSearch initialState getNeighboringStates goalTest heuristic (showOp . getOpsFromState))
  finalState <- case res of 
    Left err -> do 
      print err
      return undefined
    Right s -> pure s

  let p = fromMaybe undefined $ getPathFromState finalState
  let ops = getOpsFromState finalState

  pure (p, ops)
  where
    showOp [] = ""
    showOp (x:_) = case x of
     LMDouble y -> show y
     LMSingle y -> show y

    getNeighboringStates = exploreStates eval

    -- The goal is to find a state with a slice for each chord label.
    goalTest (SSOpen p _) = pathLen p - 1 == length chordLabels
    goalTest _ = False




plotDeriv initPath fn deriv = mapM_ printStep derivs
  where
    derivs = unfoldDerivation' initPath derivationPlayerPV deriv
    printStep el = do
      case el of
        Left err -> do
          putStrLn err
        Right g -> do
          viewGraph fn g

-- Testing Heuristic search with a simple search problem
-- mainHeuristicSearch :: IO ()
-- mainHeuristicSearch = do
--   print r
--     where
--       res = heuristicSearch initState exploreStates goalTest heuristic show
--       r = fromMaybe 0 res
--       initState = 0 :: Float
--       exploreStates n = [n+4, n-17, n+30] :: [Float]
--       goalTest = (==) (29::Float)
--       heuristic :: Float -> Float
--       heuristic x = (x-20) * (x-20)
