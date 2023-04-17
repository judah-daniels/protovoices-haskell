{-# LANGUAGE OverloadedStrings #-}
{- | This module contains -}
module Heuristics
  (
    applyHeuristic
  , heuristicZero
  , State
  )
    where

-- LOGGING
import Control.Logging qualified as Log
import Data.Text qualified as T
import Common hiding (log)

import PVGrammar hiding
  ( slicesFromFile
  )
import PVGrammar.Generate
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Language.Haskell.DoNotation
import Prelude hiding (Monad (..), log, lift, pure)

import Internal.MultiSet qualified as MS
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as S
import Data.List qualified as L
import Data.Map qualified as M

import Data.Maybe (fromJust, isNothing)
import Data.Vector qualified as V
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled

import HeuristicParser
import Harmony
import Harmony.ChordLabel
import Harmony.Params

type State ns = SearchState (Edges ns) [Edge ns] (Notes ns) (PVLeftmost ns)

applyHeuristic
  :: ((State SPitch, State SPitch) -> ExceptT String IO Double)
  -> ( State SPitch, State SPitch)
  -> ExceptT String IO Double
applyHeuristic heuristic (mprevState, state) = heuristic (mprevState, state)

testOp =
  ActionDouble
    (Start, undefined, undefined, undefined, Stop)
    $ LMDoubleSplitLeft
    $ mkSplit
    $ addToRight (c' nat) (c' nat) LeftRepeat True


log :: String -> ExceptT String IO ()
log = lift . Log.log . T.pack

heuristicZero :: ( State SPitch, State SPitch) -> ExceptT String IO Double
heuristicZero (prevState, state) = case getOpFromState state of
  Nothing -> pure 0 -- Initial state
  Just op -> do
    -- log $ "Prev State: " <> show (prevState)
    -- log $ "Next State: " <> show state
    pure $ case op of
    -- Freezing
      LMDouble doubleOp ->
        let (ms, tl, sl, tm, sr) = getParentDouble (prevState) in
          case doubleOp of
            LMDoubleFreezeLeft freezeOp -> 0
            LMDoubleSpread spreadOp ->
              let (ms', topl, stop, topr, send) = getParentDouble state
                  slcL = case sl of
                           sliceWrapped -> Just sliceWrapped
                  slcR = case sr of
                           Inner sliceWrapped -> Just sliceWrapped
                           _ -> error "nothing inside"
                in
                scoreSpread spreadOp slcL stop slcR
            LMDoubleSplitLeft splitOp ->
              let (ms', top, sr', tr', send) = getParentDouble state
                  slcL = case ms' of
                           Start -> Nothing
                           Stop -> error "Stop on left"
                           Inner sliceWrapped -> Just sliceWrapped
                  slcR = Just sr'
                in
                scoreSplit splitOp slcL top slcR
            LMDoubleSplitRight
              splitOp ->
              let (ms', tl', sl', top, send) = getParentDouble state
                  slcL = Just sl'
                  slcR = case send of
                           Stop -> Nothing
                           Start -> error "Start at on the right"
                           Inner sliceWrapped -> Just sliceWrapped
                in
                scoreSplit splitOp slcL top slcR
      LMSingle singleOp ->
        case singleOp of
          LMSingleFreeze freezeOp -> 0
          LMSingleSplit splitOp   -> 10
  where

    evaluateChordTone parentSlice n =
      case parentSlice of
        Just (SliceWrapped slc lbl prob) ->
          chordToneLogLikelihood lbl n
        Nothing -> - 100

    evaluateOrnament parentSlice n =
      case parentSlice of
        Just (SliceWrapped slc lbl prob) ->
          ornamentLogLikelihood lbl n
        Nothing -> - 100

    scoreSplit
      splitOp@( SplitOp splitReg splitPass fromLeft fromRight keepLeft keepRight passLeft passRight)
      slcL
      top
      slcR
        = let probsRS = map scoreFromLeft (allEdges fromLeft)
              probsLS = map scoreFromRight (allEdges fromRight)
              probsRegs = map scoreReg (allRegs splitReg)
              probsPassings = map scorePass (allPassings splitPass)
              probsPassLeft = map scorePassLeft (MS.toList passLeft)
              probsPassRight = map scorePassRight (MS.toList passLeft)
              aggregateProbs = probsRS <> probsLS <> probsRegs <> probsPassings <> probsPassLeft <> probsPassRight
              score = - (sum aggregateProbs / fromIntegral (length aggregateProbs))
           in
             score
      where

        scoreReg :: ((StartStop SPitch, StartStop SPitch), (SPitch, DoubleOrnament)) -> Double
        scoreReg ((Inner x,Inner y), (n,orn)) =
          case orn of
            FullNeighbor ->
              ( evaluateChordTone slcL x + evaluateChordTone slcR y
              + evaluateOrnament slcL n + evaluateOrnament slcR n) / 2

            FullRepeat ->
              ( evaluateChordTone slcL x + evaluateChordTone slcR y
              + evaluateChordTone slcL n + evaluateChordTone slcR n) / 2

            RootNote -> 0

            LeftRepeatOfRight ->
              ( evaluateChordTone slcL x + evaluateChordTone slcR y ) / 2
              + evaluateChordTone slcR n

            RightRepeatOfLeft ->
              ( evaluateChordTone slcL x + evaluateChordTone slcR y ) / 2
              + evaluateChordTone slcL n

        scoreReg ((Start,Inner y), (n,orn)) =
          case orn of
            FullNeighbor ->
              ( evaluateChordTone slcR y
              + evaluateOrnament slcL n + evaluateOrnament slcR n) / 2

            FullRepeat ->
              ( evaluateChordTone slcR y
              + evaluateChordTone slcL n + evaluateChordTone slcR n) / 2

            RootNote -> 0

            LeftRepeatOfRight ->
              evaluateChordTone slcR y / 2
              + evaluateChordTone slcR n

            RightRepeatOfLeft ->
              evaluateChordTone slcR y / 2
              + evaluateChordTone slcL n

        scoreReg ((Inner x , Stop), (n,orn)) =
          case orn of
            FullNeighbor ->
              ( evaluateChordTone slcR x
              + evaluateOrnament slcL n + evaluateOrnament slcR n) / 2

            FullRepeat ->
              ( evaluateChordTone slcR x
              + evaluateChordTone slcL n + evaluateChordTone slcR n) / 2

            RootNote -> 0

            LeftRepeatOfRight ->
              evaluateChordTone slcR x / 2
              + evaluateChordTone slcR n

            RightRepeatOfLeft ->
              evaluateChordTone slcR x / 2
              + evaluateChordTone slcL n

        scoreReg (_, (n,orn)) = error "invalid regular edge" 

        scorePass ((x,y), (n,orn)) =
          case orn of
            PassingLeft -> evaluateOrnament slcL n + evaluateChordTone slcR y
            PassingMid ->
              ( evaluateChordTone slcL x + evaluateChordTone slcR y
              + evaluateOrnament slcL n + evaluateOrnament slcR n ) / 2
            PassingRight -> evaluateOrnament slcL n + evaluateChordTone slcR x
        
        scorePassRight (n,y) = evaluateOrnament slcR n + evaluateChordTone slcR y
        scorePassLeft (x,n) = evaluateOrnament slcL n + evaluateChordTone slcL x

        scoreFromRight (n, (x,orn)) =
          case orn of
            LeftNeighbor -> evaluateOrnament slcR x + evaluateChordTone slcR n
            LeftRepeat -> evaluateChordTone slcR x + evaluateChordTone slcR n

        scoreFromLeft (n, (x,orn) ) =
          case orn of
            RightNeighbor -> evaluateOrnament slcL x + evaluateChordTone slcL n
            RightRepeat -> evaluateChordTone slcL x + evaluateChordTone slcL n

    scoreSpread
      spreadOp@(SpreadOp spreads edge@(Edges edgesReg edgesPass))
      slcL
      slc
      slcR
        = let probsRegs = map scoreReg (S.toList edgesReg)
              probsPasses = map scorePass (MS.toList edgesPass)
              aggregateProbs = probsRegs <> probsPasses
              score = - (sum aggregateProbs / fromIntegral (length aggregateProbs))
            in 
              score 
      where 
        -- scoreReg :: Edge n -> Float
        scoreReg (Start ,Inner y) = evaluateChordTone slcR y
        scoreReg (Inner x ,Stop) = evaluateChordTone slcL x 
        scoreReg (Inner x , Inner y) = evaluateChordTone slcL x + evaluateChordTone slcR y 

        -- scorePass :: InnerEdge n -> Float
        scorePass (x , y) = evaluateChordTone slcL x + evaluateChordTone slcR y

    getParentDouble
      :: State ns
      -> ( StartStop (SliceWrapped (Notes ns)) --midslice or start (in open case)
      , Edges ns                           -- tl
      , SliceWrapped (Notes ns)             -- sl 
      , Edges  ns                           -- tm
      , StartStop (SliceWrapped (Notes ns) ))-- sr or stop (2 transitions only)
    getParentDouble state = case state of
      SSFrozen _ -> error "Illegal double operation" -- SSFrozen can only be the frist state.
      SSOpen open ops ->
        case open of
          Path tl slice (Path tr sm rst) -> (Start, tContent tl, slice, tContent tr, Inner sm) -- SSOpen only case is a split from  two open transitions.
          Path tl slice (PathEnd tr) -> (Start, tContent tl, slice, tContent tr, Stop)
          PathEnd _ -> error "illegal double operation" -- SSOpen only case is a split from  two open transitions.
      SSSemiOpen frozen midSlice open ops ->
        case open of -- From two open transitions only
          Path tl slice (Path tr sm rst) -> (Inner midSlice, tContent tl, slice, tContent tr, Inner sm) -- SSOpen only case is a split from  two open transitions.
          Path tl slice (PathEnd tr) -> (Inner midSlice, tContent tl, slice, tContent tr, Stop) -- SSOpen only case is a split from  two open transitions.
          PathEnd tl -> error "Illegal double operation in the SSSemiOpen case"

    getParentSingle
      :: SearchState b es' ns o
      -> ( StartStop (SliceWrapped ns) --midslice or startstop
         , b)                          --tl
    getParentSingle state = case state of
      SSFrozen _ -> error "Illegal single operation" -- SSFrozen can only be the frist state.
      SSOpen open ops ->
        case open of
          PathEnd parent -> (Start, tContent parent) -- SSOpen only case is a split from  two open transitions.
          _ -> error "Illegal single " -- illegal state
      SSSemiOpen frozen midSlice open ops ->
        case open of -- From two open transitions only
          PathEnd parent -> (Inner midSlice, tContent parent)
          _ -> error "Illegal single" -- illegal state

    allEdges :: M.Map a [b] -> [(a, b)]
    allEdges ornamentSet = do
      (parent, children) <- M.toList ornamentSet
      child <- children
      pure (parent, child)

    allRegs :: M.Map a [b] -> [(a, b)]
    allRegs regSet = do
      (parent, children) <- M.toList regSet
      child <- children
      pure (parent, child)

    allPassings ornamentSet = do
      (parent, children) <- M.toList ornamentSet
      child <- children
      pure (parent, child)

    allOps opset = do
      (parent, children) <- M.toList opset
      child <- children
      pure (parent, child)

    allInnerEdges opset = do
      (parent, child) <- MS.toList opset
      pure (parent, child)
