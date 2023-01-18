{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HeuristicSpec where

import Musicology.Core
import Test.Hspec

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



-- import Mus
heuristicSpec :: Spec
heuristicSpec = do
  runIO $ do
    -- finalPath <- runHeuristicSearch proitoVoiceEvaluator slices321sus chords321sus
    -- slices <- slicesFromFile' "preprocessing/salamis.csv"
    -- slices <- slicesFromFile' "preprocessing/salamisShortest.csv"
    -- chords <- chordsFromFile "preprocessing/chords.csv"
    -- chords <- chordsFromFile "preprocessing/chordsShortest.csv"
    params <- loadParams "preprocessing/dcml_params.json"
    (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slices65m chords65m
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slices43 chords43
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slices321sus chords321sus
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slicesTiny chordsTiny

    -- encodeFile "outputs/ops.json" ops


    putStrLn $ show finalPath
    -- print $ testHeuristic params s 
    -- let res = evalPath finalPath chordsTiny params
    let res = evalPath finalPath chords65m params
    -- let res = evalPath finalPath chords321sus params
    putStrLn $ "\nEvaluation score: " <> show res
    -- hspec pathFromSlicesSpec
  pure ()

testOp =  
  ActionDouble 
    (Start, undefined,undefined, undefined, Stop) $
    LMDoubleSplitLeft $ mkSplit $ do 
      addToRight (c' nat) (c' nat) LeftRepeat True 




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
  [ ([(b flt 3, Music.Holds), (c nat 4, Music.Holds),(d nat 4, Music.Holds), (f shp 4, Music.Ends)], True),
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


