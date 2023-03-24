{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import Data.Maybe
  ( catMaybes,
    isNothing,
    fromMaybe,
    fromJust,
    mapMaybe,
    maybeToList,
  )
import Control.Monad.Except (ExceptT,runExceptT, lift, throwError, zipWithM)
import Data.Hashable

-- LOGGING
import qualified Data.Text as T
import Control.Logging qualified as Log

import FileHandling
import HeuristicParser
import Heuristics
import HeuristicSearch
import PBHModel
import RandomChoiceSearch
import RandomSampleParser

import Musicology.Core
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled
import PVGrammar
import PVGrammar.Parse
import Test.Hspec

import System.Environment
import System.Exit

-- COMAND LINE ARGUMENT HANDLING
parseArgs ["-h"] = usage >> exit
parseArgs ["-v"] = version >> exit
parseArgs [chordsFile, slicesFile, jsonFile, algoName] = pure (chordsFile, slicesFile, jsonFile, algoName) -- concat `fmap` mapM readFile fs
parseArgs _ = usage >> exit

usage = putStrLn 
  "\nUsage: parseFullPieces [-vh] chordsFile slicesFile jsonFile {RandomParse, RandomParseSBS, RandomSample, Heuristic1, HeuristicSBS1, all} \n\
   \   -v: Show Version \n\
   \   -h: Show Help \n\
   \   chordsFile: Path containing a csv file with the chord labels \n\
   \   slicesFile: Path containing a csv file with slices corresponding to the chord labels \n\
   \   jsonFile: Path were the json output of results should be created \n\
   \   {..}: Choose which algorithm to run. \"all\" runs all algorithms and returns results in an agregated\
   \ json file with the name of each algorithm\n"
version = putStrLn "Version 0.1"
exit = exitSuccess
die = exitWith (ExitFailure 1)


main :: IO ()
-- main = Log.withStdoutLogging fullPieceExperiment 
main = Log.withStdoutLogging perSegmentExperiment 




-- Run 3 search algorithms on the inputs given
fullPieceExperiment :: IO ()
fullPieceExperiment = Log.withStdoutLogging $ do
  Log.log "Running Full Parse"
  (chordsFile, slicesFile, jsonFile, algoName) <- getArgs >>= parseArgs
  params <- loadParams "preprocessing/dcml_params.json"
  chords <- chordsFromFile chordsFile
  print chordsFile
  slices <- slicesFromFile' slicesFile
  print slicesFile

  let wrap = SliceWrapper $ \ns -> let (r, l, p) = mostLikelyChordFromSlice params ns in SliceWrapped ns (ChordLabel l r) p

  scores <-
    evaluateSearches
      [ ("Heuristic", runHeuristicSearch params protoVoiceEvaluator wrap (applyHeuristic (testHeuristic params)))
      -- , ("HeuristicSegmented", runHeuristicSearchSingleSegments params (protoVoiceEvaluatorLimitedSize 50) wrap (applyHeuristic (testHeuristic params)) )
      , ("RandomParse", runRandomSearch params (protoVoiceEvaluatorLimitedSize 15))
      , ("RandomSample", runRandomSampleSearch)
      ]
      (scoreSegments params (scoreSegment' params))
      slices
      chords
  
  -- let scores = [("Heuristic", 3),("RandomSample", 4),("RandomParse", 7)]
  -- print jsonFile

  writeMapToJson scores jsonFile

-- Run 3 search algorithms on the inputs given
perSegmentExperiment :: IO ()
perSegmentExperiment = Log.withStdoutLogging $ do
  Log.log "Running Segment by Segment Experiment"
  (chordsFile, slicesFile, jsonFile, algoName) <- getArgs >>= parseArgs
  params <- loadParams "preprocessing/dcml_params.json"
  chords <- chordsFromFile chordsFile
  print chordsFile
  slices <- slicesFromFile' slicesFile
  print slicesFile

  let wrap = SliceWrapper $ \ns -> let (r, l, p) = mostLikelyChordFromSlice params ns in SliceWrapped ns (ChordLabel l r) p

  -- For each segment, create a new path and parse

  score <- Log.timedLog "Running Heurisic Search Segment by Segment" $ do 
    resultingSegments <- runHeuristicSearchSingleSegments params (protoVoiceEvaluatorLimitedSize 50) wrap (applyHeuristic (testHeuristic params)) slices chords

    -- Log.log $ T.pack . show $  resultingSegments
    
    -- inputSlices chordLabels
    -- let slices = pathBetweens resultingPath

    let s = scoreSegments params (scoreSegment' params) resultingSegments chords
    Log.log $ T.pack . show $  s
    pure s
  Log.log $ T.pack . show $ score

  -- scores <-
  --   evaluateSearches
  --     [ ("Heuristic", runHeuristicSearch params protoVoiceEvaluator wrap (applyHeuristic (testHeuristic params)))]
  --     (scoreSegments params (scoreSegment' params))
  --     slices
  --     chords

  let scores = [("Seg by Seg", score),("nemjef", 88)]
  -- print jsonFile
  writeMapToJson scores jsonFile

  pure ()

  -- mapM_ print scores

evaluateSearches
  :: forall bs
   . [(String, [InputSlice SPitch] -> [ChordLabel] -> IO (Path bs (Notes SPitch)))]
  -> ([Notes SPitch] -> [ChordLabel] -> Double)
  -> [InputSlice SPitch]
  -> [ChordLabel]
  -> IO [(String, Double)]
evaluateSearches algos scorer inputSlices chordLabels =
  mapM
    ( \(algoName, algo) -> Log.timedLog (T.concat ["Running ", T.pack algoName]) $ do
        resultingPath <- algo inputSlices chordLabels
        let slices = pathBetweens resultingPath
        pure (algoName, scorer slices chordLabels)
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

runRandomSampleSearch
  :: forall bs
   . bs
  -> [ChordLabel]
  -> IO (Path (Edges SPitch) (Notes SPitch))
runRandomSampleSearch _ chordLabels = do
  randomSamplePath (length chordLabels)

runRandomSearch
  :: ( Music.HasPitch ns
     , Eq (Music.IntervalOf ns)
     , Data.Hashable.Hashable ns
     , Ord ns
     , Show ns
     , Music.Notation ns
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
  showOp (x : _) = case x of
    LMDouble y -> show y
    LMSingle y -> show y

  getNeighboringStates = exploreStates idWrapper eval

  -- The goal is to find a state with a slice for each chord label.
  goalTest (SSOpen p _) = pathLen p - 1 == length chordLabels
  goalTest _ = False

-----
runHeuristicSearch
  :: ( Music.HasPitch ns
     , Eq (Music.IntervalOf ns)
     , Data.Hashable.Hashable ns
     , Ord ns
     , Show ns
     , Music.Notation ns
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
      Log.warn $ T.pack err
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
  showOp (x : _) = case x of
    LMDouble y -> show y
    LMSingle y -> show y

  getNeighboringStates = exploreStates wrap eval

  -- The goal is to find a state with a slice for each chord label.
  goalTest (SSOpen p _) = pathLen p - 1 == length chordLabels
  goalTest _ = False

runHeuristicSearchSingleSegments
  :: ( Music.HasPitch ns
     , Eq (Music.IntervalOf ns)
     , Data.Hashable.Hashable ns
     , Ord ns
     , Show ns
     , Music.Notation ns
     )
  => HarmonicProfileData
  -> Eval (Edges ns) [Edge ns] (Notes ns) [ns] (PVLeftmost ns)
  -> SliceWrapper (Notes ns)
  -> ((Maybe (State ns), State ns) -> ExceptT String IO Double)
  -> [InputSlice ns]
  -> [ChordLabel]
  -> IO [Notes ns]
runHeuristicSearchSingleSegments params eval wrap heuristic inputSlices chordLabels = do
  -- let y = pathFromSlices eval wrap inputSlices
  --   start-|-{}-{}-|-{}-{}-{}-/-{}-|-stop
  --   start-|-{}-{}-stop 
  let x = splitSlicesIntoSegments eval wrap inputSlices 

  Log.log $ T.pack ("\n"++unlines (show <$> x))
  resultingSlices <- zipWithM (runHeuristicSearchSingleSegment params eval wrap heuristic) x chordLabels

  -- let initialState = SSFrozen $ pathFromSlices eval wrap inputSlices
  -- res <- runExceptT (heuristicSearch initialState getNeighboringStates goalTest heuristic (showOp . getOpsFromState))
  -- finalState <- case res of
  --   Left err -> do
  --     Log.errorL $ T.pack err
  --   Right s -> pure s
  --
  -- let p = fromJust $ getPathFromState finalState
  -- print p

  -- Chord Guesses for evaluation with other model
  let chordGuesses =  sLbl <$> resultingSlices
  Log.log $ T.pack . show $ chordGuesses 

  -- let ops = getOpsFromState finalState

  pure $ sWContent <$> resultingSlices

 where
  -- splitPathBySegments 
  --   :: Path (Maybe [Edge ns], Bool) (SliceWrapped (Notes ns)) 
  --   -> [Path (Maybe [Edge ns], Bool) (SliceWrapped (Notes ns)) ]
  -- splitPathBySegments p = go [] p
  --   where 
  --     go :: [Path (Maybe [Edge ns], Bool) (SliceWrapped (Notes ns)) ]
  --       -> Path (Maybe [Edge ns], Bool) (SliceWrapped (Notes ns)) 
  --       -> [Path (Maybe [Edge ns], Bool) (SliceWrapped (Notes ns)) ]
  --     go ps p = case p of
  --                 PathEnd t -> 
  --
  showOp [] = ""
  showOp (x : _) = case x of
    LMDouble y -> show y
    LMSingle y -> show y

  getNeighboringStates = exploreStates wrap eval

  -- One Slice only
  goalTest (SSOpen p _) | pathLen p == 2 = True
  goalTest _ = False

runHeuristicSearchSingleSegment 
  :: ( Music.HasPitch ns
     , Eq (Music.IntervalOf ns)
     , Data.Hashable.Hashable ns
     , Ord ns
     , Show ns
     , Music.Notation ns
     )
  => HarmonicProfileData
  -> Eval (Edges ns) [Edge ns] (Notes ns) [ns] (PVLeftmost ns)
  -> SliceWrapper (Notes ns)
  -> ((Maybe (State ns), State ns) -> ExceptT String IO Double)
  -> [InputSlice ns]
  -> ChordLabel
  -> IO (SliceWrapped (Notes ns))
runHeuristicSearchSingleSegment params eval wrap heuristic inputSlices chordLabel = Log.timedLog (T.pack ("Running search on segment: \n" ++ unlines (show <$> inputSlices))) $ do
  let initialState = SSFrozen $ pathFromSlices eval wrap inputSlices
  res <- runExceptT (heuristicSearch initialState getNeighboringStates goalTest heuristic (showOp . getOpsFromState))
  finalState <- case res of
    Left err -> do
      Log.errorL $ T.pack err
    Right s -> pure s

  let p = fromJust $ getPathFromState' finalState

  let finalSlice = case pathBetweens p of 
                     [finalSlice] -> finalSlice 
                     _ -> Log.errorL "runHeuristicSearchSingleSegment: Single slice not returned by heuristic search"

  pure finalSlice

  -- print p
  --
  -- -- Chord Guesses for evaluation with other model
  -- let chordGuess =  T.pack . show  $ sLbl finalSlice
  -- Log.log chordGuess 
  --
  -- let ops = getOpsFromState finalState
  -- pure []

 where
  showOp [] = ""
  showOp (x : _) = case x of
    LMDouble y -> show y
    LMSingle y -> show y

  getNeighboringStates = exploreStates wrap eval

  -- One Slice only
  goalTest (SSOpen p _) | pathLen p == 2 = True
  goalTest _ = False
