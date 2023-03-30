{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EvaluationSpec where

import Musicology.Core
import Test.Hspec

import Debug.Trace
import Common hiding (split)
import Data.ByteString.Lazy qualified as BL
import Control.Monad.Except (ExceptT,runExceptT, lift, throwError)
import Data.Csv
import Data.Bifunctor (bimap, first, second)
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

type InputSlice ns = ([(ns, Music.RightTied)], Bool)

evaluationSpec :: Spec 
evaluationSpec = do
  runIO $ do
    -- print finalPath
    -- print $ testHeuristic params s 
    params <- loadParams "preprocessing/inputs/dcml_params.json"
    pure ()
    -- let wrap = let wrapSlice' ns = let (r,l,p) = mostLikelyChordFromSlice params ns in SliceWrapped ns (ChordLabel l r) p in SliceWrapper wrapSlice'

    -- let pathScore = scoreSegments params (scoreSegment' params) (map sWContent $ pathBetweens $ pathFromSlices protoVoiceEvaluator idWrapper slices43) chords43
    -- let pathScore' = scoreSegments params (scoreSegment' params) (map sWContent $ pathBetweens $ pathFromSlices protoVoiceEvaluator idWrapper slices43') chords43
    -- let pathScore'' = scoreSegments params (scoreSegment' params) (map sWContent $ pathBetweens $ pathFromSlices protoVoiceEvaluator idWrapper slices43'') chords43
    --
    -- let pathScore' = scoreSegments params (scoreSegment' params) (pathBetweens finalPath') chords 
    -- let pathScore'' = scoreSegments params (scoreSegment' params) (pathBetweens finalPath'') chords43
    -- let pathScore'' = scoreSegments params (scoreSegment' params) (pathBetweens finalPath'') chordsTiny
       
    -- let res = evalPath finalPath chordsTiny params
    -- let res = evalPath finalPath chords65m params
    -- let res = evalPath finalPath chords321sus params
    -- putStrLn $ "\nRandom score: " <> show pathScore
    -- putStrLn $ "\nRandom Choice score: " <> show pathScore'
    -- putStrLn "Chord CM: " 
    -- putStrLn $ "cgcf: " <> show pathScore
    -- putStrLn $ "cgce: " <> show pathScore'
    -- putStrLn $ "ceea: " <> show pathScore''

    -- print chords
    -- hspec pathFromSlicesSpec
  pure ()

slices43 :: [InputSlice SPitch]
slices43 =
  [ ([(c nat 3, Music.Holds), (g nat 4, Music.Holds),(c nat 4, Music.Holds), (f nat 4, Music.Ends)], True)
  ]

slices43' :: [InputSlice SPitch]
slices43' =
  [ ([(c nat 3, Music.Holds), (g nat 4, Music.Holds),(c nat 4, Music.Holds), (e nat 4, Music.Ends)], True)
  ]

slices43'' :: [InputSlice SPitch]
slices43'' =
  [ ([(c nat 3, Music.Holds), (e nat 4, Music.Holds),(e nat 4, Music.Holds), (a nat 4, Music.Ends)], True)
  ]

chords43 :: [ChordLabel]
chords43 =
  [ ChordLabel "M" (c' nat)
  ]

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

