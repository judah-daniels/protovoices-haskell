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
import Data.HashSet qualified as S
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
import Data.List qualified as L
import Data.Vector qualified as V
import Display
import RandomChoiceSearch
import RandomSampleParser
import FileHandling
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
import Data.Map.Strict qualified as M
import HeuristicParser 

    

fullParseSpec :: Spec 
fullParseSpec = do 
  runIO $ do
    params <- loadParams "preprocessing/dcml_params.json"
    -- slices <- slicesFromFile' "preprocessing/inputs/slices/n01op18-1_03.csv"
    -- chords <- chordsFromFile "preprocessing/inputs/chords/n01op18-1_03.csv"
    -- slices <- slicesFromFile' "preprocessing/inputs/slices/n01op18-1_04.csv"
    -- chords <- chordsFromFile "preprocessing/inputs/chords/n01op18-1_04.csv"
    slices <- slicesFromFile' "preprocessing/inputs/slices/slices1short.csv"
    chords <- chordsFromFile "preprocessing/inputs/chords/chords1short.csv"
    let wrap = SliceWrapper $ \ns -> let (r,l,p) = mostLikelyChordFromSlice params ns in SliceWrapped ns (ChordLabel l r) p

    scores <- evaluateSearches 

        [ 
        runHeuristicSearch params protoVoiceEvaluator wrap (applyHeuristic (testHeuristic params))
        ,runRandomSearch params (protoVoiceEvaluatorLimitedSize 10)
        , runRandomSampleSearch 
        ]
        (scoreSegments params (scoreSegment' params))
        slices 
        chords

    print scores
  pure ()

evaluateSearches
  :: forall bs 
  .  [[InputSlice SPitch] -> [ChordLabel] -> IO (Path bs (Notes SPitch))]
  -> ([Notes SPitch] -> [ChordLabel] -> Double)
  -> [InputSlice SPitch]
  -> [ChordLabel] 
  -> IO [Double]
evaluateSearches algos scorer inputSlices chordLabels =
   mapM 
     (\algo -> do
         resultingPath <- algo inputSlices chordLabels 
         let slices = pathBetweens resultingPath
         pure $ scorer slices chordLabels
       ) 
       algos
  

evaluateSearch 
  :: ([InputSlice SPitch] -> [ChordLabel] -> IO (Path (Edges SPitch) (Notes SPitch)))
  -> ([Notes SPitch] -> [ChordLabel] -> Double)
  -> [InputSlice SPitch]
  -> [ChordLabel] 
  -> IO Double
evaluateSearch algo scorer inputSlices chordLabels = do 
   resultingPath <- algo inputSlices chordLabels 
   let slices = pathBetweens resultingPath
   let score = scorer slices chordLabels
   pure score
  
-- import Mus
heuristicSpec :: Spec
heuristicSpec = do
  runIO $ do
    params <- loadParams "preprocessing/dcml_params.json"
    let wrap = SliceWrapper $ \ns -> let (r,l,p) = mostLikelyChordFromSlice params ns in SliceWrapped ns (ChordLabel l r) p
    slices <- slicesFromFile' "preprocessing/salamisShortest.csv"
    chords <- chordsFromFile "preprocessing/chordsShortest.csv"
    -- finalPath <- runHeuristicSearch proitoVoiceEvaluator slices321sus chords321sus
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slices321sus chords321sus
    -- slices <- slicesFromFile' "preprocessing/salamis.csv"
    -- chords <- chordsFromFile "preprocessing/chords.csv"
    -- print $ pathFromSlices protoVoiceEvaluator slices65m 
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (testHeuristic params) slices65m chords65m
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slices43 chords43

    finalPath' <- runRandomSearch params protoVoiceEvaluator slices65m chords65m
    finalPath <- runHeuristicSearch params protoVoiceEvaluator wrap (applyHeuristic (testHeuristic params)) slices65m chords65m
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slicesTiny' chordsTiny'

    -- encodeFile "outputs/ops.json" ops
    --
    -- finalPath <- runHeuristicSearch proitoVoiceEvaluator slices321sus chords321sus
    -- slices <- slicesFromFile' "preprocessing/inputs/salamis.csv"
    -- chords <- chordsFromFile "preprocessing/inputs/chords.csv"
    -- print $ pathFromSlices protoVoiceEvaluator slices65m 
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (testHeuristic params) slices65m chords65m
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slices43 chords43
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slices321sus chords321sus
    -- (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (applyHeuristic (testHeuristic params)) slicesTiny chordsTiny
    -- (finalPath'', ops) <- runHeuristicSearch params protoVoiceEvaluator wrap (applyHeuristic (testHeuristic params)) slices chords
    -- (finalPath'', ops) <- runHeuristicSearch params protoVoiceEvaluator wrap (applyHeuristic (testHeuristic params)) slices chords
    -- (finalPath'', ops) <- runHeuristicSearch params protoVoiceEvaluator wrap (applyHeuristic (testHeuristic params)) slices chords
    -- (finalPath'', ops) <- runHeuristicSearch params protoVoiceEvaluator wrap (applyHeuristic (testHeuristic params)) slices43 chords43
    -- finalPath'' <- runHeuristicSearch params protoVoiceEvaluator wrap (applyHeuristic (testHeuristic params)) slicesTiny chordsTiny
    -- finalPath <- runRandomSampleSearch chords

    -- let sliceLengths = length . fst <$> slices 
    -- mapM_ print slices
    -- print sliceLengths
    -- (finalPath', ops) <- runRandomSearch params protoVoiceEvaluator slices chords
    -- mapM_ print ops


    -- print finalPath''

    -- encodeFile "outputs/ops.json" ops

    -- print finalPath
    -- print $ testHeuristic params s 
    -- let pathScore = scoreSegments params (scoreSegment' params) (pathBetweens finalPath) chords 
    -- let pathScore' = scoreSegments params (scoreSegment' params) (pathBetweens finalPath') chords 
    -- let pathScore'' = scoreSegments params (scoreSegment' params) (pathBetweens finalPath'') chords43
    -- let pathScore'' = scoreSegments params (scoreSegment' params) (pathBetweens finalPath'') chordsTiny
       

    -- let res = evalPath finalPath chordsTiny params
    -- let res = evalPath finalPath chords65m params
    -- let res = evalPath finalPath chords321sus params
    -- putStrLn $ "\nRandom score: " <> show pathScore
    -- putStrLn $ "\nRandom Choice score: " <> show pathScore'
    -- putStrLn $ "\nHeuristic score': " <> show pathScore''

    -- print chords
    -- hspec pathFromSlicesSpec


    print finalPath
    print finalPath'
    -- print $ testHeuristic params s 
    -- let res = evalPath finalPath chordsTiny' params
    -- let res = evalPath finalPath chordsTiny params
    let res = scoreSegments params (scoreSegment params) (pathBetweens finalPath) chords65m
    let res' = scoreSegments params (scoreSegment params) (pathBetweens finalPath') chords65m
    -- let res = evalPath finalPath chords65m params
    -- let res = evalPath finalPath chords321sus params
    putStrLn $ "\nEvaluation score: " <> show res
    putStrLn $ "\nEvaluation score random: " <> show res'
    -- hspec pathFromSlicesSpec
  pure ()


-- testOp =  
--   ActionDouble 
--     (Start, undefined,undefined, undefined, Stop) $
--     LMDoubleSplitLeft $ mkSplit $ do 
--       addToRight (c' nat) (c' nat) LeftRepeat True 
---------------------------------- -------------------------------- -------------------------------- |
-- INPUTS FOR TESTING
--
-- The musical surface from Figure 4 as a sequence of slices and transitions.
-- Can be used as an input for parsing.
-- path321sus =
--   Path [e nat 4, c nat 4] [(Inner $ c nat 4, Inner $ c nat 4)] $
--     Path [d nat 4, c nat 4] [(Inner $ d nat 4, Inner $ d nat 4)] $
--       Path [d nat 4, b nat 4] [] $
--         PathEnd [c nat 4]
--
-- path43 =
--   Path [c nat 4, g nat 4, f nat 5] [(Inner $ c nat 4, Inner $ c nat 4), (Inner $ g nat 4, Inner $ g nat 4)] $
--     PathEnd [c nat 4, g nat 4, e nat 5] 
--
-- testInput :: [InputSlice SPC]
-- testInput =
--   [ ([(e' nat, Music.Holds), (c' nat, Music.Ends)], False),
--     ([(e' nat, Music.Holds)], True),
--     ([(e' nat, Music.Ends)], False)
--   ]

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
--
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
  [ ChordLabel "Mm7" (d' nat)
  ]

chords43 :: [ChordLabel]
chords43 =
  [ ChordLabel "M" (c' nat)
  ]


slicesTiny' :: [InputSlice SPitch]
slicesTiny' =
  [ ([(b flt 3, Music.Ends), (c nat 4, Music.Holds),(d nat 4, Music.Holds), (f shp 4, Music.Holds)], True),
    ([(a nat 3, Music.Ends), (c nat 4, Music.Ends),(d nat 4, Music.Ends), (f shp 4, Music.Ends)], False),
    ([(g nat 3, Music.Ends), (b flt 3, Music.Ends),(d nat 4, Music.Ends), (g nat 4, Music.Ends)], True)
  ]

chordsTiny' :: [ChordLabel]
chordsTiny' =
  [ ChordLabel "Mm7" (d' nat),
    ChordLabel "m" (g' nat)
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
  [ ChordLabel "M" (c' nat),
    ChordLabel "Mm7" (d' nat),
    ChordLabel "m" (g' nat)
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
  [ ChordLabel "M" (c' nat),
    ChordLabel "M" (g' nat),
    ChordLabel "M" (c' nat)
  ]

runRandomSampleSearch 
  :: forall bs . bs 
  -> [ChordLabel] 
  -> IO (Path (Edges SPitch) (Notes SPitch))
runRandomSampleSearch _ chordLabels = do
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
  -> IO (Path (Edges ns) (Notes ns))
runRandomSearch params eval inputSlices chordLabels = do
  let initialState = SSFrozen $ pathFromSlices eval idWrapper inputSlices
  res <- runExceptT (randomChoiceSearch initialState getNeighboringStates goalTest (showOp . getOpsFromState))
  finalState <- case res of 
    Left err -> do 
      print err
      return undefined
    Right s -> pure s

  let p = fromMaybe undefined $ getPathFromState finalState
  let ops = getOpsFromState finalState

  pure p
  where
    showOp [] = ""
    showOp (x:_) = case x of
     LMDouble y -> show y
     LMSingle y -> show y

    getNeighboringStates = exploreStates idWrapper eval

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
  -> SliceWrapper (Notes ns)
  -> ((Maybe (State ns), State ns) -> ExceptT String IO Double)
  -> [InputSlice ns] 
  -> [ChordLabel] 
  -> IO (Path (Edges ns) (Notes ns))
runHeuristicSearch params eval wrap heuristic inputSlices chordLabels = do
  let initialState = SSFrozen $ pathFromSlices eval wrap inputSlices
  res <- runExceptT (heuristicSearch initialState getNeighboringStates goalTest heuristic (showOp . getOpsFromState))
  finalState <- case res of 
    Left err -> do 
      print err
      print "something went wrong"
      return undefined
    Right s -> pure s

  let p = fromJust $ getPathFromState finalState
  print p

  -- Chord Guesses for evaluation with other model
  let chordGuesses = sLbl <$> pathBetweens (fromJust $ getPathFromState' finalState)
  mapM_ (\(ChordLabel lbl root) -> putStrLn $ showNotation root <> lbl) chordGuesses


  let ops = getOpsFromState finalState

  -- pure (p, ops)
  pure p
  where
    showOp [] = ""
    showOp (x:_) = case x of
     LMDouble y -> show y
     LMSingle y -> show y

    getNeighboringStates = exploreStates wrap eval

    -- The goal is to find a state with a slice for each chord label.
    goalTest (SSOpen p _) = pathLen p - 1 == length chordLabels
    goalTest _ = False
