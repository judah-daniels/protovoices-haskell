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
import Data.Aeson qualified as A 

-- COMAND LINE ARGUMENT HANDLING
parseArgs ["-h"] = usage >> exit
parseArgs ["-v"] = version >> exit
parseArgs [chordsFile, slicesFile, jsonFile, algoName] = pure (chordsFile, slicesFile, jsonFile, read algoName) -- concat `fmap` mapM readFile fs
parseArgs _ = usage >> exit

data ParseAlgo 
  = RandomParse
  | RandomParseSBS
  | RandomSample 
  | Heuristic1 
  | HeuristicSBS1 
  | All
  deriving (Read, Show, Eq)

usage = putStrLn 
  "\nUsage: parseFullPieces [-vh] chordsFile slicesFile jsonFile {RandomParse, RandomParseSBS, RandomSample, Heuristic1, HeuristicSBS1, All} \n\
   \   -v:         Show Version \n\
   \   -h:         Show Help \n\
   \   chordsFile: Path containing a csv file with the chord labels \n\
   \   slicesFile: Path containing a csv file with slices corresponding to the chord labels \n\
   \   jsonFile:   Output path for results \n\
   \   {..}:       Choose which algorithm to run. \"all\" runs all algorithms and returns results in an aggregated\
   \ json file\n"
version = putStrLn "Version 0.1"
exit = exitSuccess
die = exitWith (ExitFailure 1)

data AlgoResult = AlgoResult [Notes SPitch] [ChordLabel] (Maybe (Path (Edges SPitch) (Notes SPitch)) ) Double Double
   deriving (Show)

main :: IO ()
main = Log.withStdoutLogging $ do 
  params <- loadParams "preprocessing/dcml_params.json"
  (chordsFile, slicesFile, jsonFile, algo) <- getArgs >>= parseArgs
  inputChords <- chordsFromFile chordsFile
  inputSlices <- slicesFromFile' slicesFile

  res <- let runAlgo = case algo of 
              RandomParse -> runRandomParse protoVoiceEvaluator
              -- RandomParseSBS -> runRandomParseSBS 
              -- RandomSample -> runRandomSample p
              -- Heuristic1 -> runHeuristic1 params 
              -- HeuristicSBS1 -> runHeuristicSBS1 params 
              -- All -> runAllAlgos params 
   in runAlgo params inputChords inputSlices jsonFile 

  case res of 
    AlgoResult sl ch pa ac li -> writeJSONToFile jsonFile $ writeResultsToJSON sl ch pa ac li 

  pure ()
    
{- | Runs a random search through the entire piece
     This can get stuck due to combinatoral blowup. 
     This can also reach a deadend
-}
runRandomParse 
  :: Eval (Edges SPitch) [Edge SPitch] (Notes SPitch) [SPitch] (PVLeftmost SPitch)
  -> HarmonicProfileData
  -> [ChordLabel] 
  -> [InputSlice SPitch] 
  -> String 
  -> IO AlgoResult
runRandomParse eval params chords inputSlices jsonFile = Log.timedLog "Running Random Parse" $ do 
  let initialState = SSFrozen $ pathFromSlices eval idWrapper inputSlices
  res <- runExceptT 
    (randomChoiceSearch initialState (exploreStates idWrapper eval) (goalTest chords) (showOp . getOpsFromState))

  finalState <- case res of
    Left err -> do
      print err
      return undefined
    Right s -> pure s

  let path = fromJust $ getPathFromState finalState
  let slices = pathBetweens path

  let scorer = scoreSegments params (scoreSegment' params)
  let score = scorer slices chords

  let wrap = SliceWrapper $ \ns -> let (r, l, p) = mostLikelyChordFromSlice params ns in SliceWrapped ns (ChordLabel l r) p
  
  let chordGuesses =  sLbl <$> (wrapSlice wrap <$> slices)
  
  pure $ AlgoResult slices chordGuesses Nothing score score 

  -- writeJSONToFile jsonFile $ writeResultsToJSON slices chords (getPathFromState finalState) 43 3 

  -- writeMapToJson scores jsonFile
  
-- writeMapToJson :: [(String, Double)] -> FilePath -> IO ()
-- writeMapToJson dict fileName = do 
--   -- fileHandle <- openFile fileName
--   let json = A.toJSON (M.fromList dict)
--   Log.debug $ T.pack . show $ json
--   BL.writeFile fileName (A.encode json)
--   -- closeFile fileHandle

  -- let ops = getOpsFromState finalState
  

{- | Runs a random search within each segment
-}
-- runRandomParseSBS :: [ChordLabel] -> [InputSlice SPitch] -> String -> IO ()
-- runRandomParseSBS chordsFile slicesFile jsonFile = do 
--   pure ()
--
-- {- | Samples random notes for every segment, without looking at the segment itself 
-- -}
-- runRandomSample :: [ChordLabel] -> [InputSlice SPitch] -> String -> IO ()
-- runRandomSample chordsFile slicesFile jsonFile = do 
--   pure ()
--
-- {- | Samples random notes from each segment
-- -}
-- runRandomSampleSBS :: [ChordLabel] -> [InputSlice SPitch] -> String -> IO ()
-- runRandomSampleSBS chordsFile slicesFile jsonFile = do 
--   pure ()
--
--
-- {- | Uses a beam search, using chordtone and ornamentation probabilities as a score
-- -}
-- runHeuristic1 :: HarmonicProfileData -> [ChordLabel] -> [InputSlice SPitch] -> String -> IO ()
-- runHeuristic1 params chordsFile slicesFile jsonFile = do 
--   pure ()
--
-- {- | Uses a beam search, using chordtone and ornamentation probabilities as a score, but running separately for each segment
-- -}
-- runHeuristicSBS1 :: HarmonicProfileData -> [ChordLabel] -> [InputSlice SPitch] -> String -> IO ()
-- runHeuristicSBS1 params chordsFile slicesFile jsonFile = do 
--   pure ()
--
-- runAllAlgos :: HarmonicProfileData -> [ChordLabel] -> [InputSlice SPitch] -> String -> IO ()
-- runAllAlgos params chordsFile slicesFile jsonFile = do 
--   pure ()
--






-- Run 3 search algorithms on the inputs given
fullPieceExperiment :: String -> String -> String -> IO ()
fullPieceExperiment chordsFile slicesFile jsonFile = Log.withStdoutLogging $ do
  Log.log "Running Full Parse"
  -- (chordsFile, slicesFile, jsonFile, _) <- getArgs >>= parseArgs
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
perSegmentExperiment :: String -> String -> String -> IO ()
perSegmentExperiment chordsFile slicesFile jsonFile = Log.withStdoutLogging $ do
  Log.log "Running Segment by Segment Experiment"
  -- (chordsFile, slicesFile, jsonFile, _) <- getArgs >>= parseArgs
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
  writeMapToJson scores jsonFile
  -- print jsonFile

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
  res <- runExceptT (randomChoiceSearch initialState getNeighboringStates (goalTest chordLabels) (showOp . getOpsFromState))
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
  res <- runExceptT (heuristicSearch initialState getNeighboringStates (goalTest chordLabels) heuristic (showOp . getOpsFromState))
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
  res <- runExceptT (heuristicSearch initialState getNeighboringStates goalTestSBS heuristic (showOp . getOpsFromState))
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
  getNeighboringStates = exploreStates wrap eval



showOp [] = ""
showOp (x : _) = case x of
  LMDouble y -> show y
  LMSingle y -> show y

-- One Slice only
goalTestSBS (SSOpen p _) | pathLen p == 2 = True
goalTestSBS _ = False

-- The goal is to find a state with a slice for each chord label.
goalTest chordLabels (SSOpen p _) = pathLen p - 1 == length chordLabels
goalTest chordlabels _ = False
