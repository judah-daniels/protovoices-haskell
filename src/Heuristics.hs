module Heuristics where

import Common

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

type State = SearchState (Edges SPitch) [Edge SPitch] (Notes SPitch) (PVLeftmost SPitch)

applyHeuristic
  :: ( Maybe (PVLeftmost SPitch )-> State -> ExceptT String IO Float)
  -> State 
  -> IO Float
applyHeuristic heuristic state = do
  res <- runExceptT $ heuristic op state
  case res of
    Left err -> do
      print err
      pure 1000.0
    Right s -> pure s
 where
  ops = getOpsFromState state

  op = case ops of
    [] -> Nothing
    x : _ -> Just x

  remainingOps :: Float
  remainingOps = fromIntegral $ getPathLengthFromState state

-- ActionDouble (⋊,|{},{F5,C5,A3},|{F5-F5,C5-C5},{F5,C5,G4}) (LMDoubleSplitLeft regular:{}, passing:{}, ls:{}, rs:{[A4:LeftRepeat]<=A3,[C5:LeftRepeat]<=C5,[F3:LeftRepeat,F5:LeftRepeat]<=F5}, kl:{}, kr:{F5-F5,C5-C5}, pl:{}, pr:{})
testOp =  
  ActionDouble 
    (Start, undefined,undefined, undefined, Stop) $
    LMDoubleSplitLeft $ mkSplit $ do 
      addToRight (c' nat) (c' nat) LeftRepeat True 

testHeuristic :: HarmonicProfileData -> Maybe (PVLeftmost SPitch )-> State -> ExceptT String IO Float
testHeuristic params op state =
  if isNothing op
    then pure 100.0
    else case fromJust op of
      -- Freezing
      -- no unfreeze if there are 3 open non boundary transition
      LMDouble (LMDoubleFreezeLeft freezeOp) -> do
        (parent, _, _, _) <- getParentDouble state
        case applyFreeze freezeOp parent of
          Left err -> throwError err
          Right frozen -> pure 0.2

      -- Spreading
      -- Calculate for each chord possibility, the likelihood that they are all the same chord
      -- Splitting Right
      {-
                              split:
      ..=[_]----pl----[slc]-==----pr---[_]....
            \cl      /     \          /cr
             \      /       \        /
              [slcl]----cm---[ slcr ]
      -}
      LMDouble (LMDoubleSpread spreadOp@(SpreadOp spreads edges)) -> do
        (parentl, slc, parentr, _) <- getParentDouble state
        case applySpread spreadOp parentl slc parentr of
          Left err -> throwError err
          Right (childl, slcl, childm, slcr, childr) -> do
            lift $ print "Evaluating Spread operation:"
            -- lift $ print (childl,slcl,childm,slcr,childr)
            -- lift $ print (parentl, slc, parentr)
            -- lift $ print "parent:"
            let (root, chordType, cProb) = mostLikelyChordFromSlice params slc
            -- lift $ print "left:"
            -- lift $ print $ mostLikelyChordFromSlice params slcl
            -- lift $ print "right:"
            -- lift $ print $ mostLikelyChordFromSlice params slcr
            let mu = sliceChordLogLikelihood params (mkLbl root chordType) (transformSlice slc)
            let wu = sliceChordLogLikelihood params (mkLbl root chordType) (transformSlice slcl)
            let ru = sliceChordLogLikelihood params (mkLbl root chordType) (transformSlice slcr)
            let pu = mu + wu - ru - ru
            lift $ print $ "mu: " <> show mu
            lift $ print $ "val: " <> show pu

            pure 2.0
           where
            -- trace (show $ mostLikelyChordFromSlice params parent ) -2.0

            mkLbl root chordType = ChordLabel chordType root (spc 0)
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
        parent <- getParentSingle state
        case applyFreeze freezeOp parent of
          Left err -> throwError err
          Right frozen -> pure 0.2

      -- Splitting Only
      {-
                                      split:
                             ..=[mS]--parent---end
                                  cl\        /cr
                                      [slc]
      -}
      LMSingle (LMSingleSplit splitOp) -> do
        parent <- getParentSingle state
        case applySplit splitOp parent of
          Left err -> trace err undefined
          Right (childl, slc, childr) -> pure 1.0
      -- Splitting Left
      {-
                                      split:
                             ..=[mS]--parent---[slcl]--tr--[slcr]...
                                  cl\        /cr
                                      [slc]
      -}
      LMDouble (LMDoubleSplitLeft splitOp) -> do
        lift $ print "Splitting Left"
        (parent, slcl, tr, slcr) <- getParentDouble state
        case applySplit splitOp parent of
          Left err -> trace err undefined
          Right (childl, slice, childr) -> pure 2.0

      -- Splitting Right
      {-
                                      split:
              ..=[mS]--tl---[slcl]---parent---[slcr]....
                               cl\           /cr
                                  \         /
                                    [ slc ]
      -}
      LMDouble (LMDoubleSplitRight splitOp) -> do
        (tl, slcl, parent, slcr) <- getParentDouble state
        case applySplit splitOp parent of
          Left err -> throwError err
          Right (childl, slice, childr) -> pure 1.5
 where
  getParentDouble state = case state of
    SSFrozen _ -> throwError "ERRORMSG" -- SSFrozen can only be the frist state.
    SSOpen open ops ->
      case open of
        Path tl slice (Path tr sm rst) -> pure (tContent tl, sContent slice, tContent tr, sContent sm) -- SSOpen only case is a split from  two open transitions.
        Path tl slice (PathEnd _) -> throwError "illegal double spread" -- illegal?
        PathEnd _ -> throwError "illegal double spread" -- SSOpen only case is a split from  two open transitions.
    SSSemiOpen frozen (Slice midSlice) open ops ->
      case open of -- From two open transitions only
        Path tl slice (Path tr sm rst) -> pure (tContent tl, sContent slice, tContent tr, sContent sm) -- SSOpen only case is a split from  two open transitions.
        Path tl slice (PathEnd tr) -> pure (tContent tl, sContent slice, tContent tr, undefined) -- SSOpen only case is a split from  two open transitions.
        PathEnd tl -> throwError "This case I believe never occurs?"

  getParentSingle state = case state of
    SSFrozen _ -> throwError "wtf" -- SSFrozen can only be the frist state.
    SSOpen open ops ->
      case open of
        PathEnd parent -> pure $ tContent parent -- SSOpen only case is a split from  two open transitions.
        _ -> throwError "Illegal single " -- illegal state
    SSSemiOpen frozen (Slice midSlice) open ops ->
      case open of -- From two open transitions only
        PathEnd parent -> pure $ tContent parent
        _ -> throwError "Illegal single" -- illegal state
