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
import Probability

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

import Data.Maybe (fromJust, isNothing, maybeToList, mapMaybe, listToMaybe)
import Data.Vector qualified as V
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled

import Parser.HeuristicParser
import Harmony
import Harmony.ChordLabel
import Harmony.Params
import Debug.Trace
import Streamly.Internal.Data.Array.Foreign.Mut (fromForeignPtrUnsafe)
import Musicology.Pitch.Class (transpose)

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
log = lift . Log.debug . T.pack

-- | Basic heuristic for protovoice operations: 
--   Consists of two factors: P(n|p,l), the probability of each child note considering the parent notes. 
--   P(p|l) the plausibility of the parent notes being chord tones of the corresponding parent slices.

heuristicZero :: Double -> Double -> ( State SPitch, State SPitch) -> ExceptT String IO Double
heuristicZero alpha splitWeight (prevState, state) = case getOpFromState state of
  Nothing -> pure 0 -- Initial state
  Just op -> do
    log $ "Prev State: " <> show (prevState)
    log $ "Next State: " <> show state
    let t = evalOP op
    log $ show op
    log $ show t
    pure t
  where
    evalOP op = case op of
      LMDouble doubleOp ->
        let (ms, tl, sl, tm, sr) = getParentDouble prevState in
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
                -- trace ("split: " <> (show $ scoreSplit splitOp slcL top slcR)) scoreSplit splitOp slcL top slcR
                scoreSplit splitOp slcL top slcR
      LMSingle singleOp ->
        case singleOp of
          LMSingleFreeze freezeOp -> 0
          LMSingleSplit splitOp   -> 10


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

    scoreParents
      :: Notes SPitch
      -> Notes SPitch
      -> Maybe (SliceWrapped (Notes ns))
      -> Maybe (SliceWrapped (Notes ns))
      -> Double
    scoreParents leftParents rightParents slcL slcR =
      let (probl, probr) =
            ((\(SliceWrapped lns sLbl prob) -> prob) <$> slcL
            ,(\(SliceWrapped lns sLbl prob) -> prob) <$> slcR)
          (lbll, lblr) =
            ((\(SliceWrapped lns sLbl prob) -> sLbl) <$> slcL
            ,(\(SliceWrapped lns sLbl prob) -> sLbl) <$> slcR)
          leftScore' = scoreParent (probsParent lbll) leftParents
          rightScore' = scoreParent (probsParent lblr) rightParents
          leftScore = case probl of 
                         Nothing -> [] 
                         Just a -> [a ]
          rightScore = case probr of 
                         Nothing -> [] 
                         Just a -> [a ]
          -- scoreParent (probsParent lblr) rightParents
          bothScores =  (leftScore ++ rightScore ++ leftScore' ++ rightScore')
          n = fromIntegral $ length bothScores
        in
          if n == 0 then 0 else sum bothScores / n
        where
          scoreParent mps v = do
            multinomialLogProb (notesVector v) <$> mps

    scoreChildren
      :: [(SPitch, DoubleOrnament)]
      -> [(SPitch, PassingOrnament)]
      -> [(SPitch, RightOrnament)]
      -> [(SPitch, LeftOrnament)]
      -> Maybe (SliceWrapped nx)
      -> Maybe (SliceWrapped nx)
      -> Double
    scoreChildren childRegs childPasses childFromLefts childFromRights slcL slcR =
      let (lbll, lblr) =
            ((\(SliceWrapped _ sLbl _) -> sLbl) <$> slcL
            ,(\(SliceWrapped _ sLbl _) -> sLbl) <$> slcR)
          -- regMixtures = (probsRegs lbll lblr) <$> childRegs 
          scoresRegs = mapMaybe (scoreChild (probsRegs lbll lblr)) childRegs
          scoresPasses = mapMaybe (scoreChild (probsPassings lbll lblr)) childPasses
          scoresFromLeft = mapMaybe (scoreChild (probsFromLeft lbll)) childFromLefts
          scoresFromRight = mapMaybe (scoreChild (probsFromRight lblr)) childFromRights
          allScores = scoresRegs <> scoresPasses <> scoresFromLeft <> scoresFromRight
          n = fromIntegral $ length allScores
        in
         if n == 0 then -100 else sum allScores / fromIntegral (length allScores)
      where
        scoreChild v (n, orn) = maximumMaybe $ scoreChildList v (n,orn)

        scoreChildList v (n, orn) = do
          res <- v orn
          maybeToList $ categoricalLogProb (fifths n) res


    -- Need to find all the parent on the left of the child slice 
    -- And all the parents form the right of the child slice
    scoreSplit
      splitOp@( SplitOp splitReg splitPass fromLeft fromRight keepLeft keepRight passLeft passRight)
      slcL
      top
      slcR
        = let leftRegParents = allRegLeftParents splitReg
              rightRegParents = allRegRightParents splitReg
              leftPassingParents = allPassingLeftParents splitPass
              rightPassingParents = allPassingRightParents splitPass
              fromLeftParents = allSingleParents fromLeft
              fromRightParents = allSingleParents fromRight

              -- Parents are all evaluated using chord-tone profiles
              leftParents = leftRegParents <> leftPassingParents <> fromLeftParents
              rightParents = rightRegParents <> rightPassingParents <> fromRightParents
              

              -- Single children are evaluated using the profiles from parents/ chordtone or ornament
              childFromRights = allSingleChildren fromRight
              childFromLefts = allSingleChildren fromLeft

              -- Regs are evaluated from a mixture from both parents
              childRegs = allDoubleChildren splitReg

              --Passing children are evaluated from ornment profiles of both parents - mixture
              childPasses = allDoubleChildren splitPass

              parentFactor =
                scoreParents
                  (Notes $ MS.fromList leftParents)
                  (Notes $ MS.fromList rightParents) slcL slcR

              childFactor = scoreChildren childRegs childPasses childFromLefts childFromRights slcL slcR
              score = - (childFactor*alpha + parentFactor) -- *(defaultUnsplitBias) -- parentFactor bug
           in
             score

    scoreSpread
      :: Spread SPitch
      -> Maybe (SliceWrapped (Notes SPitch))
      -> SliceWrapped (Notes SPitch)
      -> Maybe (SliceWrapped (Notes SPitch))
      -> Double
    scoreSpread
      spreadOp@(SpreadOp spreads edge@(Edges edgesReg edgesPass))
      slcL
      slc
      slcR
        = let (lbll, lblr) = ((\(SliceWrapped _ sLbl probl) -> probl) <$> slcL ,(\(SliceWrapped _ sLbl probr) -> probr) <$> slcR)
              childPasses = (\(l, r) -> (l, PassingMid)) <$> allInnerEdges edgesPass
              childRegs = (\(Inner l,r) -> (l, FullRepeat)) <$> allRegEdges edgesReg
              childFactor = scoreChildren childRegs childPasses [] [] slcL slcR
              --     (Notes $ MS.fromList rightParents) slcL slcR
           -- in trace ("spread: " <> (show $ (-50 * go lbll lblr) )) (- go lbll lblr) * 50
           in - (childFactor * alpha + (go slcL slcR))
          where
            go Nothing Nothing = 100
            go (Just a) Nothing = 100
            go Nothing (Just a) = 100
            go (Just a ) (Just b) = ((scoreParents (sWContent a) (sWContent b) (slcL) (slcR)))
          -- let
          -- probsRegs = map scoreReg (S.toList edgesReg)
              -- probsPasses = map scorePass (MS.toList edgesPass)
              -- aggregateProbs = probsRegs <> probsPasses
              -- l = length aggregateProbs
              -- score = if l == 0 then 5 else - (sum aggregateProbs / fromIntegral (length aggregateProbs))
            -- in
        -- scoreReg :: Edge n -> Float
        -- scoreReg (Start ,Inner y) = evaluateChordTone slcR y
        -- scoreReg (Inner x ,Stop) = evaluateChordTone slcL x
        -- scoreReg (Inner x , Inner y) = evaluateChordTone slcL x + evaluateChordTone slcR y
        --
        -- -- scorePass :: InnerEdge n -> Float
        -- scorePass (x , y) = evaluateChordTone slcL x + evaluateChordTone slcR y

    -- scoreParents
    --   :: Notes SPitch
    --   -> Notes SPitch
    --   -> Maybe (SliceWrapped nx)
    --   -> Maybe (SliceWrapped nx)
    --   -> Double
    -- scoreParents leftParents rightParents slcL slcR =
    --   let (lbll, lblr) =
    --         ((\(SliceWrapped _ sLbl _) -> sLbl) <$> slcL
    --         ,(\(SliceWrapped _ sLbl _) -> sLbl) <$> slcR)
    --       leftScore = scoreParent (probsParent lbll) leftParents
    --       rightScore = scoreParent (probsParent lblr) rightParents
    --       bothScores =  (maybeToList leftScore ++ maybeToList rightScore)
    --       n = fromIntegral $ length bothScores
    --     in
    --       if n == 0 then 0 else sum bothScores / n
    --     where
    --       scoreParent mps v = do
    --         multinomialLogProb (notesVector v) <$> mps
    --
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

    allSingleParents :: M.Map a [b] -> [a]
    allSingleParents ornamentSet = do
      (parent, children) <- M.toList ornamentSet
      pure parent

    allSingleChildren :: M.Map a [b] -> [b]
    allSingleChildren ornamentSet = do
      (parent, children) <- M.toList ornamentSet
      children

    allRegs :: M.Map a [b] -> [(a, b)]
    allRegs regSet = do
      (parent, children) <- M.toList regSet
      child <- children
      pure (parent, child)

    allDoubleChildren :: M.Map a [b] -> [b]
    allDoubleChildren regSet = do
      (parent, children) <- M.toList regSet
      children

    allRegLeftParents :: M.Map (Edge n) [b] -> [n]
    allRegLeftParents regSet = do
      ((l,_), ornament ) <- M.toList regSet
      maybeToList $ startStopToMaybe l

    allRegRightParents :: M.Map (Edge n) [b] -> [n]
    allRegRightParents regSet = do
      ((_,r), ornament ) <- M.toList regSet
      maybeToList $ startStopToMaybe r

    allPassingLeftParents :: M.Map (InnerEdge n) [b] -> [n]
    allPassingLeftParents regSet = do
      ((l,_), ornament ) <- M.toList regSet
      pure l

    allPassingRightParents :: M.Map (InnerEdge n) [b] -> [n]
    allPassingRightParents regSet = do
      ((_,r), ornament ) <- M.toList regSet
      pure r

    allPassingChildren :: M.Map a [b] -> [b]
    allPassingChildren regSet = do
      (parent, children) <- M.toList regSet
      children
    startStopToMaybe Start = Nothing
    startStopToMaybe Stop = Nothing
    startStopToMaybe (Inner a) = Just a


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

    allRegEdges opset = do
      (parent, child) <- S.toList opset
      pure (parent, child)

maximumMaybe :: (Ord a, Foldable f) => f a -> Maybe a
maximumMaybe xs
  | null xs   = Nothing
  | otherwise = Just $ maximum xs

-- Workaround cus this made the heuristic worse
enharmonicLabels :: ChordLabel -> [ChordLabel]
enharmonicLabels lbl = [lbl]
 where
  shiftProfileLeft lbl@(ChordLabel chordType rootNote) =
    let rootNote' = transpose (sic 12) rootNote
     in ChordLabel chordType rootNote'

  shiftProfileRight lbl@(ChordLabel chordType rootNote) =
    let rootNote' = transpose (sic (-12)) rootNote
     in ChordLabel chordType rootNote'
