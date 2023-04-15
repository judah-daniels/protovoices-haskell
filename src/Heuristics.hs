{-# LANGUAGE OverloadedStrings #-}
{- | This module contains -}
module Heuristics 
  ( 
    applyHeuristic
  , testHeuristic
  , State 
  )
    where

-- LOGGING
import Control.Logging qualified as Log
import Data.Text qualified as T
import Common hiding (log)
import Internal.MultiSet qualified as MS
import PVGrammar hiding
  ( slicesFromFile
  )
import PVGrammar.Generate
import PVGrammar.Parse
import Control.Monad (foldM)
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Language.Haskell.DoNotation
import Prelude hiding (Monad (..), log, lift, pure)
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
  :: ((Maybe (State SPitch), State SPitch) -> ExceptT String IO Double)
  -> (Maybe (State SPitch), State SPitch)
  -> ExceptT String IO Double
applyHeuristic heuristic (prevState, state) = do
  heuristic (prevState, state)
 where
  remainingOps :: Double
  remainingOps = fromIntegral $ getPathLengthFromState state

testOp =
  ActionDouble
    (Start, undefined, undefined, undefined, Stop)
    $ LMDoubleSplitLeft
    $ mkSplit
    $ do
      addToRight (c' nat) (c' nat) LeftRepeat True


log = lift . Log.log . T.pack

-- | First Heuristic Prototype
testHeuristic :: (Maybe (State SPitch), State SPitch) -> ExceptT String IO Double
testHeuristic (prevState, state) = do
  case getOpFromState state of
    Nothing -> pure 0 -- Initial state
    Just op -> 
      do 
      log "______________________________________________________"
      log $ "Prev State: " <> show (fromJust prevState)
      log $ "Next State: " <> show state

      case op of
      -- Freezing
        LMDouble (LMDoubleFreezeLeft freezeOp) -> pure 0
        LMSingle (LMSingleFreeze freezeOp) -> pure 0
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

        LMDouble (LMDoubleSpread spreadOp) -> do
          log "Considering an unspread"
          res <- getParentDouble (fromJust prevState)
          (_, childl, slcl, childr, slcr) <- case res of
            (sl, childl, Inner slc, childr, Inner sr) -> pure (sl, childl, slc, childr, sr)
            (_, _, Start, _, _) -> throwError "StartStop left of in spread"
            (_, _, Stop, _, _) -> throwError "StartStop in left of spread"
            (_, _, _, _, Start) -> throwError "StartStop in right of spread"
            (_, _, _, _, Stop) -> throwError "StartStop in right of spread"

          -- pure 30000
          scoreSpread slcl slcr spreadOp

        -- Freezing (Terminate)
        -- numSlices From  SSFrozen with 1 or 1+ transitions

        -- Splitting Only
        {-
                                        split:
                               ..=[mS]--parent---end
                                    cl\        /cr
                                        [slc]
        -}
        LMSingle (LMSingleSplit splitOp) -> do
          log "Considering a single unsplit"
          -- (child, sr)
          (slcl, parent) <- getParentSingle state
          -- (slcl, parent, slcr, _, _) <- getParentDouble (fromJust prevState)
          -- pure 10
          scoreSplit splitOp parent slcl slcl Stop

        -- Splitting Left
        {-
                                        split:
                               ..=[ms]--parent---[slcl]--_--[slcr_...
                                      cl\        /cr
                                            [slc]
          -}
        LMDouble (LMDoubleSplitLeft splitOp) -> do
          log "Considering an unsplit left"

          log "parent slices"
          (ms, slcl, slcr) <- getParentSlices (fromJust prevState)
          log $ "MidSlice: " <> show ms
          log $ "sl: " <> show slcl
          log $ "sr: " <> show slcr

          (childSlice, parent, _, _, _) <- getParentDouble state
          log $ "ChildSlice: " <> show childSlice
          log $ show parent
          -- pure 5

          case childSlice of 
            Inner c -> pure $ - sLblProb c
            _ -> pure 12
          -- scoreSplit splitOp parent childSlice ms slcl

        -- Splitting Right
        {-
                                        split:
                ..=[mS]--tl---[slcl]---parent---[slcr]....
                                 cl\           /cr
                                    \         /
                                      [ slc ]
                                        split:
                ..=x--tl---[slcl]---parent------x-
                               cl\           /cr
                                  \         /
                                    [ slc ]
        -}
        LMDouble (LMDoubleSplitRight splitOp) -> do
          log "Considering an unsplit right"
          let sSlices = fromJust $ getSlicesFromState state
          let pSlices = fromJust $ getSlicesFromState (fromJust prevState) -- DISGUSTING

          (ms, slcl, slcr) <- 
                case sSlices of 
                  (ms:slcl:slcr:rst) -> pure (ms,slcl, slcr)
                  _ -> throwError "idk"

          
          (ms', slcl', slc, slcr') <- 
                case pSlices of 
                  (ms':slcl':slc:slcr':rst) -> pure (ms',slcl', slc, slcr')
                  _ -> throwError "idk"

          -- log $ show ms 
          -- log $ show slcl 
          -- log $ show slcr 
          -- log $ show ms' 
          -- log $ show slcl' 
          -- log $ show slc 
          -- log $ show slcr' 
          pure $ - sLblProb slc 

          -- (_,_, sl, _, child) <- getParentDouble (fromJust prevState)
          -- (_, _, slcl, parent, slcr) <- getParentDouble state
          -- pure 5
          -- scoreSplit splitOp parent child slcl slcr
   where
    getParentSlices' :: State ns -> State ns -> ExceptT String IO (StartStop (Notes ns), StartStop (Notes ns))
    getParentSlices' prevState state = do
      let sSlices = fromJust $ getSlicesFromState state
      let pSlices = getSlicesFromState prevState

      -- =[mS]---[sl]
      case state of 
        SSSemiOpen f m s o -> case pSlices of 
                                Just (ms:sl:sr : rst) -> undefined
                                Just _ -> error "getParentSlices: not enough slices"
                                Nothing -> undefined
        SSOpen s o -> undefined
        SSFrozen f -> error "getting parent slice of frozen"

      let p = fromJust $ getPathFromState state
      let slices = pathBetweens p
      case slices of
        (sl : sr : rst) -> pure (Inner sl, Inner sr)
        _ -> throwError "How can you even unsplit if there arent two slices? Confusion"

    -- Scoring Split
    {-
                                  split:
                       ....[sl]-----parent---[sr]....
                             cl\           /cr
                                \         /
                               [ slc child ]
    -}

    getParentSlices 
      :: State ns 
      -> ExceptT 
          String 
          IO 
          ( StartStop (SliceWrapped (Notes ns))
          , StartStop (SliceWrapped (Notes ns))
          , StartStop (SliceWrapped (Notes ns))
          )
    getParentSlices s = do
      let p = fromJust $ getPathFromState' s
      let slices = pathBetweens p
      let midSlice = case getSlicesFromState s of 
                       Nothing -> Start
                       Just (ms:rst) -> Inner ms

      case slices of
        (sl : sr : rst) -> pure (midSlice, Inner sl, Inner sr)
        _ -> throwError "How can you even unsplit if there arent two slices? Confusion"

    getParentTrans
      :: State ns 
      -> ExceptT 
          String 
          IO 
          (Path (Edges ns) (SliceWrapped (Notes ns)))
    getParentTrans s = do
      let p = fromJust $ getPathFromState' s
      pure p

    scoreSpread
      :: (Music.Notation n)
      => SliceWrapped (Notes n)
      -> SliceWrapped (Notes n)
      -> Spread SPitch
      -> ExceptT String IO Double
    scoreSpread (SliceWrapped slcl lblL@(ChordLabel ctl rl) probL) (SliceWrapped slcr lblR@(ChordLabel _ rr) probR) spreadOp@(SpreadOp spreads edges) = do
      pure 3
     --  log "Considering an unspread"
     --  let slc = MS.fromList $ HM.keys spreads
     --  let slc' = Notes $ MS.map (transposeNote rl) slc
     --  log "Left parent"
     --  log $ show slcl
     --  log $ show lblL <> " " <> show probL
     --  log "Right parent"
     --  log $ show slcr
     --  log $ show lblR <> " " <> show probR
     --  log "Child parent"
     --  log $ show (MS.toList slc)
     --  let wu = sliceChordLogLikelihood params lblL slc'
     --  let ru = sliceChordLogLikelihood params lblL slc'
     --  let ml = probL
     --  let mr = probR
     --
     --  let score = -(wu + ru + (ml + mr) / 2 )
     --  -- let score = - 4
     --  log $ "score: " <> show score
     --  pure score
     -- where
     --  mkLbl root chordType = ChordLabel chordType (spc (root - 14))
     --  go :: SpreadDirection -> Double
     --  go = undefined
     --
    scoreSplit
      :: (Music.Notation n, Show a)
      => Split SPitch
      -> Edges n
      -> a
      -> StartStop (SliceWrapped (Notes n))
      -> StartStop (SliceWrapped (Notes n))
      -> ExceptT String IO Double
    scoreSplit
      splitOp@(SplitOp splitRegs splitPassings ls rs keepl keepr passl passr)
      parent@(Edges topRegs topPassings)
      childSlice
      slcl
      slcr = do
        pure 0
       --  lblL <- case slcl of
       --    Start -> pure Nothing
       --    Stop -> throwError "Stop on Left?"
       --    Inner (SliceWrapped (Notes _) lbl prob) -> pure $ Just (lbl, prob)
       --  lblR <- case slcr of
       --    Start -> throwError "Start on Right?"
       --    Stop -> pure Nothing
       --    Inner (SliceWrapped (Notes _) lbl prob) -> pure $ Just (lbl, prob)
       --
       --  probsRS <- mapM (scoreLeftOrnament lblR) (allOrnaments rs)
       --  probsLS <- mapM (scoreRightOrnament lblL) (allOrnaments ls)
       --  probsRegs <- mapM (scoreRegs lblL lblR) (allRegs splitRegs)
       --  probsPassings <- mapM (scorePassing lblL lblR) (allPassings splitPassings)
       --
       --  log $ "\n\tOperation Contents:\n" <> show splitOp
       --
       --  log $ "\nLeft parent slice: " <> show slcl
       --  log $ "Inferred Chord: " <> show lblL
       --  log $ "\nRight parent slice: " <> show slcr
       --  log $ "Inferred Chord: " <> show lblR
       --  log $ "\nChild slice: " <> show childSlice
       --
       --  log $ "Left slice ornaments:" <> showOps opLs
       --  log $ "scores: " <> show probsLS
       --  log $ "Right slice ornaments:" <> showOps opRs
       --  log $ "scores: " <> show probsRS
       --
       --  log $ "Regular edges:" <> show (allRegs splitRegs)
       --  log $ "scores: " <> show probsRegs
       --  log $ "Passing edges:" <>  show (allPassings splitPassings)
       --  log $ "scores: " <> show probsPassings
       --  
       --  let aggregateProbs = probsRS <> probsLS <> probsRegs <> probsPassings
       --
       --
       --  log $ "Operation scores: " <> show aggregateProbs
       --
       --  let score = -(sum aggregateProbs / fromIntegral (L.length aggregateProbs))
       --  log $ "score: " <> show score
       --  pure score
       -- where
       --  scoreRightOrnament
       --    :: Maybe (ChordLabel, Double)
       --    -> (SPitch, (SPitch, RightOrnament))
       --    -> ExceptT String IO Double
       --  scoreRightOrnament Nothing (n, (x, orn)) = throwError "Right Ornament with no parent"
       --  scoreRightOrnament (Just (lbl@(ChordLabel chordLbl root), prob)) (n, (x, orn)) = do
       --    log $ "Scoring a right ornament: " <> Music.showNotation n <> "<="<> Music.showNotation x
       --    let (n', x') = (transposeNote root n, transposeNote root x)
       --     in case orn of
       --          RightNeighbor -> do
       --            log "Right Neighbor"
       --            log $ "LBL: " <> show (Just (lbl, prob))
       --            log $ "Chord tone likelihood: " <> show (chordToneLogLikelihood params lbl n')
       --            log $ "Ornament tone likelihood: " <> show (ornamentLogLikelihood params lbl x')
       --            pure $ chordToneLogLikelihood params lbl n' + ornamentLogLikelihood params lbl x'
       --          RightRepeat -> do
       --            log "Right Repeat"
       --            log $ "LBL: " <> show (Just (lbl, prob))
       --            log $ "Chord tone likelihood: " <> show (chordToneLogLikelihood params lbl n')
       --            pure $ 2 * chordToneLogLikelihood params lbl n'
       --
       --  scoreLeftOrnament
       --    :: Maybe (ChordLabel, Double)
       --    -> (SPitch, (SPitch, LeftOrnament))
       --    -> ExceptT String IO Double
       --  scoreLeftOrnament Nothing (n, (x, orn)) = throwError "Left Ornament with no parent"
       --  scoreLeftOrnament (Just (lbl@(ChordLabel chordType rootNote), prob)) (n, (x, orn)) = do
       --    log $ "Scoring a left ornament: " <> Music.showNotation x <> "=>"<> Music.showNotation n
       --    let (n', x') = (transposeNote root n, transposeNote root x)
       --     in case orn of
       --          LeftNeighbor -> do
       --            log "Left Neighbor"
       --            log $ "LBL: " <> show (Just (lbl, prob))
       --            log $ "Chord tone likelihood: " <> show (chordToneLogLikelihood params lbl n')
       --            log $ "Ornament tone likelihood: " <> show (ornamentLogLikelihood params lbl x')
       --            pure $ chordToneLogLikelihood params lbl n' + ornamentLogLikelihood params lbl x'
       --          LeftRepeat -> do
       --            log "Left Repeat"
       --            log $ "LBL: " <> show (Just (lbl, prob))
       --            log $ "Chord tone likelihood: " <> show (chordToneLogLikelihood params lbl n')
       --            pure $ 2 * chordToneLogLikelihood params lbl n'
       --
       --  scoreRegs Nothing _ ((x, y), (n, orn)) = throwError "Attempted to score a regular edge without a chord label"
       --  scoreRegs _ Nothing ((x, y), (n, orn)) = throwError "Attempted to score a regular edge without a chord label"
       --  scoreRegs (Just (lbll@(ChordLabel chordlbll rootl), probL)) (Just (lblr@(ChordLabel chordlblr rootr), probR)) ((Inner nl, Inner nr), (x, orn)) =
       --    let (nl', nr') = (transposeNote rootl nl, transposeNote rootr nr)
       --     in case orn of
       --          FullNeighbor ->
       --            pure $
       --              (chordToneLogLikelihood params lbll nl' + chordToneLogLikelihood params lblr nr') / 2
       --                + ornamentLogLikelihoodDouble params lbll lblr x
       --          FullRepeat ->
       --            pure $
       --              (chordToneLogLikelihood params lbll nl' + chordToneLogLikelihood params lblr nr') / 2
       --                + chordToneLogLikelihoodDouble params lbll lblr x
       --          RootNote -> pure 0
       --          LeftRepeatOfRight -> pure $ chordToneLogLikelihood params lblr nr'
       --          RightRepeatOfLeft -> pure $ chordToneLogLikelihood params lblr nl'
       --
       --  scorePassing Nothing _ ((x, y), (n, orn)) = throwError "Attempted to score a passing edge without a chord label"
       --  scorePassing _ Nothing ((x, y), (n, orn)) = throwError "Attempted to score a passing edge without a chord label"
       --  scorePassing (Just (lbll@(ChordLabel chordlbll rootl), probL)) (Just (lblr@(ChordLabel chordlblr rootr), probR)) ((nl, nr), (x, orn)) =
       --    let (nl', nr') = (transposeNote rootl nl, transposeNote rootr nr)
       --     in case orn of
       --          PassingLeft -> pure $ ornamentLogLikelihood params lblr (transposeNote rootl x) + chordToneLogLikelihood params lblr nr'
       --          PassingMid -> pure $ ornamentLogLikelihoodDouble params lbll lblr x + (chordToneLogLikelihood params lblr nr' + chordToneLogLikelihood params lblr nl') / 2
       --          PassingRight -> pure $ ornamentLogLikelihood params lblr (transposeNote rootr x) + chordToneLogLikelihood params lbll nl'
       --
       --  opLs = showL <$> M.toList ls
       --  opRs = showR <$> M.toList rs
       --
       --
       --  showOps ops = "\n   " <> L.intercalate "\n   " ops
       --
       --  showEdge (p1, p2) = Music.showNotation p1 <> "-" <> Music.showNotation p2
       --  showEdges ts = "{" <> L.intercalate "," (showEdge <$> S.toList ts) <> "}"
       --  showChild (p, o) = Music.showNotation p <> ":" <> show o
       --  showChildren cs = "[" <> L.intercalate "," (showChild <$> cs) <> "]"
       --
       --  showSplit (e, cs) = showEdge e <> "=>" <> showChildren cs
       --  showL (p, lchilds) = Music.showNotation p <> "=>" <> showChildren lchilds
       --  showR (p, rchilds) = showChildren rchilds <> "<=" <> Music.showNotation p
       --
       --  allOrnaments :: M.Map a [b] -> [(a, b)]
       --  allOrnaments ornamentSet = do
       --    (parent, children) <- M.toList ornamentSet
       --    child <- children
       --    pure (parent, child)
       --
       --  allRegs regSet = do
       --    (parent, children) <- M.toList regSet
       --    child <- children
       --    pure (parent, child)
       --
       --  allPassings ornamentSet = do
       --    (parent, children) <- M.toList ornamentSet
       --    child <- children
       --    pure (parent, child)
       --
       --  allOps opset = do
       --    (parent, children) <- M.toList opset
       --    child <- children
       --    pure (parent, child)
       --
       --  applyRegs top ops = do
       --    (top', notes) <- foldM (applyReg top) (top, MS.empty) $ allOps ops
       --    if S.null top'
       --      then pure notes
       --      else throwError $ "did not use all terminal edges, remaining: " <> showEdges top'
       --
       --  applyReg topAll (top, notes) (parent, (note, _))
       --    | parent `S.member` topAll =
       --        pure (top', notes')
       --    | otherwise =
       --        throwError $
       --          "used non-existing terminal edge\n  top="
       --            <> show parent
       --            <> "\n  split="
       --            <> show splitOp
       --   where
       --    top' = S.delete parent top
       --    notes' = MS.insert note notes
       --
       --  singleChild (_, (note, _)) = note
       --  collectNotes ops = MS.fromList $ singleChild <$> allOps ops
       --
    getParentDouble state = case state of
      SSFrozen _ -> throwError "Illegal double operation" -- SSFrozen can only be the frist state.
      SSOpen open ops ->
        case open of
          Path tl slice (Path tr sm rst) -> pure (Start, tContent tl, Inner slice, tContent tr, Inner sm) -- SSOpen only case is a split from  two open transitions.
          Path tl slice (PathEnd tr) -> pure (Start, tContent tl, Inner slice, tContent tr, Stop)
          PathEnd _ -> throwError "illegal double operation" -- SSOpen only case is a split from  two open transitions.
      SSSemiOpen frozen midSlice open ops ->
        case open of -- From two open transitions only
          Path tl slice (Path tr sm rst) -> pure (Inner midSlice, tContent tl, Inner slice, tContent tr, Inner sm) -- SSOpen only case is a split from  two open transitions.
          Path tl slice (PathEnd tr) -> pure (Inner midSlice, tContent tl, Inner slice, tContent tr, Stop) -- SSOpen only case is a split from  two open transitions.
          PathEnd tl -> throwError "Illegal double operation in the SSSemiOpen case"

    getParentSingle state = case state of
      SSFrozen _ -> throwError "Illegal single operation" -- SSFrozen can only be the frist state.
      SSOpen open ops ->
        case open of
          PathEnd parent -> pure (Start, tContent parent) -- SSOpen only case is a split from  two open transitions.
          _ -> throwError "Illegal single " -- illegal state
      SSSemiOpen frozen midSlice open ops ->
        case open of -- From two open transitions only
          PathEnd parent -> pure (Inner midSlice, tContent parent)
          _ -> throwError "Illegal single" -- illegal state
