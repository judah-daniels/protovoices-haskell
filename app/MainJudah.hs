{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE RebindableSyntax #-}

module Main where

import Debug.Trace
import Common
import Data.ByteString.Lazy qualified as BL
import Control.Monad.Except (ExceptT, lift, throwError)
import Data.Csv
import Data.List.Split
import Data.Hashable
import Data.Maybe
  ( catMaybes,
    isNothing,
    fromMaybe ,
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
import Control.Monad.Except (runExceptT)

type InputSlice ns = ([(ns, Music.RightTied)], Bool)

main :: IO ()
main = do
  -- slices <- slicesFromFile' "preprocessing/salamis.csv"
  slices <- slicesFromFile' "preprocessing/salamisShortest.csv"
  -- chords <- chordsFromFile "preprocessing/chords.csv"
  chords <- chordsFromFile "preprocessing/chordsShortest.csv"
  params <- loadParams "preprocessing/dcml_params.json"
  -- finalPath <- runHeuristicSearch protoVoiceEvaluator slices321sus chords321sus
  (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (testHeuristic params) slices chords

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
  -> (SearchState (Edges ns) [Edge ns] (Notes ns) (PVLeftmost ns) -> IO Float)
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



testHeuristic
  :: --(Hashable ns, Show ns, Eq (Music.IntervalOf ns), Music.HasPitch ns, Music.Notation ns, Ord ns)
  HarmonicProfileData
  -> SearchState (Edges SPitch) [Edge SPitch] (Notes SPitch) (PVLeftmost SPitch)
  -> IO Float
testHeuristic params state = do 
    res <- runExceptT score 
    case res of 
        Left err -> do 
          print err
          pure 1000.0
        Right s -> pure s
    where
      ops = getOpsFromState state

      op = case ops of 
             [] -> Nothing
             x:_ -> Just x

      remainingOps :: Float
      remainingOps = fromIntegral $ getPathLengthFromState state 

      score :: ExceptT String IO Float
      score = if isNothing op then pure 100.0 else 
        case fromJust op of
          -- Splitting Left
          LMDouble (LMDoubleSplitLeft splitOp) -> do 
            lift $ print "Splitting Left"
            let (tl, parent, tr) = getParentDouble state 
            case applySplit splitOp tl of 
              Left err -> trace err undefined
              Right (childl,slice,childr) -> pure 2.0
          
          -- Splitting Right
          LMDouble (LMDoubleSplitRight splitOp) -> do

            let (tl, parent, tr) = getParentDouble state
            case applySplit splitOp tr of 
              Left err -> trace err undefined
              Right (childl,slice,childr) -> pure 1.5

          -- Freezing
          -- no unfreeze if there are 3 open non boundary transition 
          LMDouble (LMDoubleFreezeLeft freezeOp) -> do
            let (tl, parent, tr) = getParentDouble state
            case applyFreeze freezeOp tl of 
              Left err -> trace err undefined
              Right frozen -> pure  0.2 

          -- Spreading
          -- Calculate for each chord possibility, the likelihood that they are all the same chord
          LMDouble (LMDoubleSpread spreadOp@(SpreadOp spreads edges)) -> do
            let (tl, parent, tr) = getParentDouble state 
            case applySpread spreadOp tl parent tr of 
              Left err -> trace err undefined
              Right (sl,childl,slice,childr,st) -> pure 2.0
                -- trace (show $ mostLikelyChordFromSlice params parent ) -2.0
                where 
                  -- predChord = mostLikelyChord hpData parent 
                  -- jointLogLikelihood = 
                  --     sliceChordLogLikelihood childl 
                  --   + sliceChordLogLikelihood childr 
                  --   - sliceChordLogLikelihood predChord

                  -- jointLikelihood
                  go :: SpreadDirection -> Float
                  go = undefined




          -- Freezing (Terminate)
          -- numSlices From  SSFrozen with 1 or 1+ transitions
          LMSingle (LMSingleFreeze freezeOp) -> do
            let parent = getParentSingle state
            case applyFreeze freezeOp parent of 
              Left err -> trace err undefined
              Right frozen -> pure 0.2 

          -- Splitting Only
{-
                                split:          
                       ..=[mS]--parent---end
                            cl\        /cr                     
                                [slc]                       
-}
          LMSingle (LMSingleSplit splitOp) -> do 
            let parent = getParentSingle state
            case applySplit splitOp parent of 
              Left err -> trace err undefined
              Right (childl, slc, childr) -> pure 1.0
         where 
           getParentDouble state = case state of 
              SSFrozen _ -> undefined -- SSFrozen can only be the frist state.
              SSOpen open ops -> 
                case open of 
                  Path tl slice (Path tr sm rst) -> (tContent tl, sContent slice,  tContent tr)  -- SSOpen only case is a split from  two open transitions. 
                  Path tl slice (PathEnd _ ) -> trace "illegal double spread" undefined  -- illegal?
                  PathEnd _ -> trace "illegal double spread" undefined  -- SSOpen only case is a split from  two open transitions. 

              SSSemiOpen frozen (Slice midSlice) open ops -> 
                case open of -- From two open transitions only 
                  Path tl slice (Path tr sm rst) -> (tContent tl, sContent slice, tContent tr)  -- SSOpen only case is a split from  two open transitions. 
                  Path tl slice (PathEnd tr) -> (tContent tl, sContent slice, tContent tr)  -- SSOpen only case is a split from  two open transitions. 
                  PathEnd tl -> trace "This case I believe never occurs?" (tContent tl, undefined, undefined)

           getParentSingle state = tContent $ case state of 
              SSFrozen _ -> undefined -- SSFrozen can only be the frist state.
              SSOpen open ops -> 
                case open of 
                  PathEnd parent -> parent  -- SSOpen only case is a split from  two open transitions. 
                  _ -> trace "Illegal single " undefined -- illegal state

              SSSemiOpen frozen (Slice midSlice) open ops -> 
                case open of -- From two open transitions only 
                  PathEnd parent -> parent 
                  _ -> trace "Illegal single" undefined -- illegal state


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
