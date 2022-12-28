{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE RebindableSyntax #-}

module Main where

import Common
import Data.ByteString.Lazy qualified as BL
import Data.Csv
import Data.List.Split
import Data.Hashable
import Data.Maybe
  ( catMaybes,
    fromMaybe,
    mapMaybe,
    maybeToList,
  )
import Data.Vector qualified as V
import Display
import Evaluator
import GHC.IO.Handle.Lock (FileLockingNotSupported (FileLockingNotSupported))
import HeuristicParser
import HeuristicSearch
import Language.Haskell.DoNotation
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled
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

type InputSlice ns = ([(ns, Music.RightTied)], Bool)

main :: IO ()
main = do
  finalPath <- runHeuristicSearch protoVoiceEvaluator slices321sus chords321sus
  hpData <- loadParams "preprocessing/dcml_params.json"

  let res = evalPath finalPath chords321sus hpData

  putStrLn $ "\nFinal Path: " <> show finalPath
  putStrLn $ "\nEvaluation score: " <> show res

---------------------------------- -------------------------------- -------------------------------- |
-- INPUTS FOR TESTING
--
-- The musical surface from Figure 4 as a sequence of slices and transitions.
-- Can be used as an input for parsing.
path321sus =
  Path [e' nat, c' nat] [(Inner $ c' nat, Inner $ c' nat)] $
    Path [d' nat, c' nat] [(Inner $ d' nat, Inner $ d' nat)] $
      Path [d' nat, b' nat] [] $
        PathEnd [c' nat]

testInput :: [InputSlice SPC]
testInput =
  [ ([(e' nat, Music.Holds), (c' nat, Music.Ends)], False),
    ([(e' nat, Music.Holds)], True),
    ([(e' nat, Music.Ends)], False)
  ]

slices321sus :: [InputSlice SPC]
slices321sus =
  [ ([(e' nat, Music.Ends), (c' nat, Music.Holds)], True),
    ([(d' nat, Music.Holds), (c' nat, Music.Ends)], False),
    ([(d' nat, Music.Holds), (b' nat, Music.Ends)], False),
    ([(c' nat, Music.Ends)], True)
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
      <*> r .: "chordtype"
      <*> r .: "rootoffset"
      <*> r .: "globalkey"

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
    parseChordLabel r = Evaluator.ChordLabel (_chordtype r) (Music.sic $ _rootoffset r) (fromMaybe undefined (Music.readNotation $ _globalkey r))

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
    sPitch = Data.Maybe.fromMaybe undefined (Music.readNotation $ _pitch s) -- Refactor to deal with maybe

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
    mkPath eLeft [] = PathEnd (Nothing, False)

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
  ) =>
  Eval (Edges ns) [Edge ns] (Notes ns) [ns] (PVLeftmost ns) ->
  [InputSlice ns] ->
  [ChordLabel] ->
  IO (Path (Edges ns) (Notes ns))
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
