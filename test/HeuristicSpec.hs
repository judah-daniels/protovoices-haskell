{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HeuristicSpec where

import Musicology.Core
import Test.Hspec

import Debug.Trace
import Common hiding (split)
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
import HeuristicParser
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

-- ([(Note, Tied?)], Start of new segment)
type InputSlice ns = ([(ns, Music.RightTied)], Bool)

fullParseSpec :: Spec 
fullParseSpec = do 
  runIO $ do
    -- finalPath <- runHeuristicSearch proitoVoiceEvaluator slices321sus chords321sus
    -- slices <- slicesFromFile' "preprocessing/inputs/salamis.csv"
    slices <- slicesFromFile' "preprocessing/inputs/salamisShort.csv"
    -- chords <- chordsFromFile "preprocessing/inputs/chords.csv"
    chords <- chordsFromFile "preprocessing/inputs/chordsShort.csv"
    -- print $ pathFromSlices protoVoiceEvaluator slices65m 
    params <- loadParams "preprocessing/inputs/dcml_params.json"
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (testHeuristic params) slices65m chords65m
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slices43 chords43
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slices321sus chords321sus
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slicesTiny chordsTiny
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slices chords
    finalPath <- runRandomSampleSearch chords

    -- let sliceLengths = length . fst <$> slices 
    -- mapM_ print slices
    -- print sliceLengths
    -- (finalPath, ops) <- runRandomSearch params protoVoiceEvaluator slices chords
    -- mapM_ print ops
    -- print finalPath

    -- encodeFile "outputs/ops.json" ops

    -- print finalPath
    -- print $ testHeuristic params s 
    let pathScore = evalPath finalPath chords params 
       

    -- let res = evalPath finalPath chordsTiny params
    -- let res = evalPath finalPath chords65m params
    -- let res = evalPath finalPath chords321sus params
    putStrLn $ "\nEvaluation score: " <> show pathScore

    -- print chords
    -- hspec pathFromSlicesSpec
  pure ()


-- import Mus
heuristicSpec :: Spec
heuristicSpec = do
  runIO $ do
    -- finalPath <- runHeuristicSearch proitoVoiceEvaluator slices321sus chords321sus
    -- slices <- slicesFromFile' "preprocessing/salamis.csv"
    -- slices <- slicesFromFile' "preprocessing/salamisShortest.csv"
    -- chords <- chordsFromFile "preprocessing/chords.csv"
    -- chords <- chordsFromFile "preprocessing/chordsShortest.csv"
    -- print $ pathFromSlices protoVoiceEvaluator slices65m 
    params <- loadParams "preprocessing/dcml_params.json"
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (testHeuristic params) slices65m chords65m
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slices43 chords43
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slices321sus chords321sus
    (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slicesTiny chordsTiny
    -- (finalPath, ops) <- runRandomSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slicesTiny chordsTiny
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slicesTiny' chordsTiny'

    -- encodeFile "outputs/ops.json" ops


    print finalPath
    -- print $ testHeuristic params s 
    -- let res = evalPath finalPath chordsTiny' params
    -- let res = evalPath finalPath chordsTiny params
    let res = scoreSegments (pathBetweens finalPath) chordsTiny params
    -- let res = evalPath finalPath chords65m params
    -- let res = evalPath finalPath chords321sus params
    putStrLn $ "\nEvaluation score: " <> show res
    -- hspec pathFromSlicesSpec
  pure ()

testOp =  
  ActionDouble 
    (Start, undefined,undefined, undefined, Stop) $
    LMDoubleSplitLeft $ mkSplit $ do 
      addToRight (c' nat) (c' nat) LeftRepeat True 

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

path43 =
  Path [c nat 4, g nat 4, f nat 5] [(Inner $ c nat 4, Inner $ c nat 4), (Inner $ g nat 4, Inner $ g nat 4)] $
    PathEnd [c nat 4, g nat 4, e nat 5] 

testInput :: [InputSlice SPC]
testInput =
  [ ([(e' nat, Music.Holds), (c' nat, Music.Ends)], False),
    ([(e' nat, Music.Holds)], True),
    ([(e' nat, Music.Ends)], False)
  ]

slices43 :: [InputSlice SPitch]
slices43 =
  [ ([(c nat 3, Music.Holds), (g nat 4, Music.Holds),(c nat 4, Music.Holds), (f nat 4, Music.Ends)], True),
    ([(c nat 3, Music.Ends), (g nat 4, Music.Ends),(c nat 4, Music.Ends), (e nat 4, Music.Ends)], False)
  ]

slices65m :: [InputSlice SPitch]
slices65m =
  [ ([(b flt 3, Music.Ends), (c nat 4, Music.Holds),(d nat 4, Music.Holds), (f shp 4, Music.Holds)], True),
    ([(a nat 3, Music.Ends), (c nat 4, Music.Ends),(d nat 4, Music.Ends), (f shp 4, Music.Ends)], False)
  ]

slices65m' :: [InputSlice SPitch]
slices65m' =
  [ ([(b flt 3, Music.Ends), (c nat 4, Music.Ends),(d nat 4, Music.Ends), (f shp 4, Music.Ends)], True),
    ([(a nat 3, Music.Ends), (c nat 4, Music.Ends),(d nat 4, Music.Ends), (f shp 4, Music.Ends)], False)
  ]
slices65m'' :: [InputSlice SPitch]
slices65m'' =
  [ ([(c shp 3, Music.Ends), (g nat 4, Music.Ends),(b nat 4, Music.Ends), (e flt 4, Music.Ends)], True),
    ([(a nat 3, Music.Ends), (c nat 4, Music.Ends),(d nat 4, Music.Ends), (f shp 4, Music.Ends)], False)
  ]
chords65m :: [ChordLabel]
chords65m =
  [ ChordLabel "Mm7" (sic 0) (d' nat)
  ]

chords43 :: [ChordLabel]
chords43 =
  [ ChordLabel "M" (sic 1) (f' nat)
  ]


slicesTiny' :: [InputSlice SPitch]
slicesTiny' =
  [ ([(b flt 3, Music.Ends), (c nat 4, Music.Holds),(d nat 4, Music.Holds), (f shp 4, Music.Holds)], True),
    ([(a nat 3, Music.Ends), (c nat 4, Music.Ends),(d nat 4, Music.Ends), (f shp 4, Music.Ends)], False),
    ([(g nat 3, Music.Ends), (b flt 3, Music.Ends),(d nat 4, Music.Ends), (g nat 4, Music.Ends)], True)
  ]

chordsTiny' :: [ChordLabel]
chordsTiny' =
  [ ChordLabel "Mm7" (sic 3) (f' nat),
    ChordLabel "m" (sic 2) (f' nat)
  ]

slicesTiny :: [InputSlice SPitch]
slicesTiny =
  [ ([(c nat 3, Music.Holds), (g nat 4, Music.Holds),(c nat 4, Music.Holds), (f nat 4, Music.Ends)], False),
    ([(c nat 3, Music.Ends), (g nat 4, Music.Ends),(c nat 4, Music.Ends), (e nat 4, Music.Ends)], False),
    ([(b flt 3, Music.Ends), (c nat 4, Music.Holds),(d nat 4, Music.Holds), (f shp 4, Music.Holds)], True),
    ([(a nat 3, Music.Ends), (c nat 4, Music.Holds),(d nat 4, Music.Ends), (f shp 4, Music.Ends)], False),
    ([(g nat 3, Music.Ends), (b flt 3, Music.Ends),(d nat 4, Music.Ends), (g nat 4, Music.Ends)], True)
  ]

chordsTiny :: [ChordLabel]
chordsTiny =
  [ ChordLabel "M" (sic 1) (f' nat),
    ChordLabel "Mm7" (sic 3) (f' nat),
    ChordLabel "m" (sic 2) (f' nat)
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

runRandomSampleSearch 
  :: [ChordLabel] 
  -> IO (Path () (Notes SPitch))
runRandomSampleSearch chordLabels = do
  randomSamplePath (length chordLabels)


runRandomSearch ::
  ( Music.HasPitch ns,
    Eq (Music.IntervalOf ns),
    Data.Hashable.Hashable ns,
    Ord ns,
    Show ns,
    Music.Notation ns
  ) 
  => HarmonicProfileData 
  -> Eval (Edges ns) [Edge ns] (Notes ns) [ns] (PVLeftmost ns) 
  -> [InputSlice ns] 
  -> [ChordLabel] 
  -> IO (Path (Edges ns) (Notes ns), [PVLeftmost ns])
runRandomSearch params eval inputSlices chordLabels = do
  let initialState = SSFrozen $ pathFromSlices eval inputSlices
  res <- runExceptT (randomChoiceSearch initialState getNeighboringStates goalTest (showOp . getOpsFromState))
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

-----
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
  -> ((Maybe (State ns), State ns) -> ExceptT String IO Double)
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

pathFromSlices ::
  forall ns o.
  Eval (Edges ns) [Edge ns] (Notes ns) [ns] o ->
  [InputSlice ns] ->
  Path (Maybe [Edge ns], Bool) (Slice (Notes ns))
pathFromSlices eval = reversePath . mkPath False Nothing
  where
    mkPath ::
      Bool ->
      Maybe [Edge ns] ->
      [InputSlice ns] ->
      Path (Maybe [Edge ns], Bool) (Slice (Notes ns))

    mkPath tie eLeft [] = PathEnd (eLeft, True)

    mkPath tie eLeft ((slice, boundary) : rst) =
      Path (eLeft, boundary) nextSlice $
        mkPath False (Just $ getTiedEdges slice) rst
          where 
            nextSlice = Slice $ evalSlice eval (fst <$> slice)


    getTiedEdges :: [(ns, Music.RightTied)] -> [Edge ns]
    getTiedEdges = mapMaybe mkTiedEdge
      where
        mkTiedEdge :: (ns, Music.RightTied) -> Maybe (Edge ns)
        mkTiedEdge (p, Music.Holds) = Just (Inner p, Inner p)
        mkTiedEdge _ = Nothing

