{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module PVGrammar.Prob.Simple where

import           Common
import           PVGrammar
import           PVGrammar.Generate

import           Control.Monad                  ( guard
                                                , unless
                                                , when
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State      ( StateT
                                                , execStateT
                                                )
import qualified Data.Bifunctor                as Bi
import           Data.Foldable                  ( forM_ )
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as S
import           Data.Hashable                  ( Hashable )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           GHC.Generics                   ( Generic )
import           Inference.Conjugate
import qualified Internal.MultiSet             as MS
import           Lens.Micro.TH                  ( makeLenses )
import           Musicology.Pitch              as MP
                                         hiding ( a
                                                , b
                                                , c
                                                , d
                                                , e
                                                , f
                                                , g
                                                )

data PVParamsOuter f = PVParamsOuter
  { _pSingleFreeze     :: f Beta
  , _pDoubleLeft       :: f Beta
  , _pDoubleLeftFreeze :: f Beta
  , _pDoubleRightSplit :: f Beta
  }
  deriving Generic

deriving instance (Show (f Beta)) => Show (PVParamsOuter f)

makeLenses ''PVParamsOuter

data PVParamsInner f = PVParamsInner
  -- split
  { _pElaborateRegular              :: f Beta
  , _pElaborateL                    :: f Beta
  , _pElaborateR                    :: f Beta
  , _pRootFifths                    :: f Beta
  , _pKeepL                         :: f Beta
  , _pKeepR                         :: f Beta
  , _pRepeatOverNeighbor            :: f Beta
  , _pNBChromatic                   :: f Beta
  , _pNBAlt                         :: f Beta
  , _pRepeatLeftOverRight           :: f Beta
  , _pConnect                       :: f Beta
  , _pConnectChromaticLeftOverRight :: f Beta
  , _pPassLeftOverRight             :: f Beta
  , _pNewPassingLeft                :: f Beta
  , _pNewPassingRight               :: f Beta
  -- hori
  , _pNewPassingMid                 :: f Beta
  , _pNoteHoriDirection             :: f (Dirichlet 3)
  , _pNotesOnOtherSide              :: f Beta
  , _pHoriRepetitionEdge            :: f Beta
  }
  deriving Generic

deriving instance ( Show (f Beta)
                  , Show (f Beta)
                  , Show (f Beta)
                  , Show (f (Dirichlet 3))
                  , Show (f Beta)
                  ) => Show (PVParamsInner f)

makeLenses ''PVParamsInner

data PVParams f = PVParams
  { _pOuter :: PVParamsOuter f
  , _pInner :: PVParamsInner f
  }
  deriving Generic

deriving instance ( Show (f Beta)
                  , Show (f Beta)
                  , Show (f Beta)
                  , Show (f (Dirichlet 3))
                  , Show (f Beta)
                  ) => Show (PVParams f)

makeLenses ''PVParams

type PVProbs = PVParams ProbsRep
type PVProbsInner = PVParamsInner ProbsRep

type ContextSingle n = (StartStop (Notes n), Edges n, StartStop (Notes n))
type ContextDouble n
  = (StartStop (Notes n), Edges n, Notes n, Edges n, StartStop (Notes n))

type PVObs a = StateT (Trace PVParams) (Either String) a

sampleDerivation' :: _ => m (Either String [PVLeftmost SPC])
sampleDerivation' =
  sampleDerivation $ PathEnd (Edges (S.singleton (Start, Stop)) MS.empty)

obsDerivation' :: [PVLeftmost SPC] -> Either String (Trace PVParams)
obsDerivation' deriv =
  obsDerivation deriv $ PathEnd (Edges (S.singleton (Start, Stop)) MS.empty)

sampleDerivation
  :: _ => Path (Edges SPC) (Notes SPC) -> m (Either String [PVLeftmost SPC])
sampleDerivation top = runExceptT $ go Start top False
 where
  go sl surface ars = case surface of
    -- 1 trans left:
    PathEnd t -> do
      step <- lift $ sampleSingleStep (sl, t, Stop)
      case step of
        LMSingleSplit splitOp -> do
          (ctl, cs, ctr) <- except $ applySplit splitOp t
          nextSteps      <- go sl (Path ctl cs (PathEnd ctr)) False
          pure $ LMSplitOnly splitOp : nextSteps
        LMSingleFreeze freezeOp -> pure [LMFreezeOnly freezeOp]
    -- 2 trans left
    Path tl sm (PathEnd tr) -> goDouble sl tl sm tr Stop ars PathEnd
    -- 3 or more trans left
    Path tl sm (Path tr sr rest) ->
      goDouble sl tl sm tr (Inner sr) ars (\tr' -> Path tr' sr rest)

  -- helper for the two cases of 2+ edges (2 and 3+):
  goDouble sl tl sm tr sr ars mkrest = do
    step <- lift $ sampleDoubleStep (sl, tl, sm, tr, sr) ars
    case step of
      LMDoubleSplitLeft splitOp -> do
        (ctl, cs, ctr) <- except $ applySplit splitOp tl
        nextSteps      <- go sl (Path ctl cs (Path ctr sm (mkrest tr))) False
        pure $ LMSplitLeft splitOp : nextSteps
      LMDoubleFreezeLeft freezeOp -> do
        nextSteps <- go (Inner sm) (mkrest tr) False
        pure $ LMFreezeLeft freezeOp : nextSteps
      LMDoubleSplitRight splitOp -> do
        (ctl, cs, ctr) <- except $ applySplit splitOp tr
        nextSteps      <- go sl (Path tl sm (Path ctl cs (mkrest ctr))) True
        pure $ LMSplitRight splitOp : nextSteps
      LMDoubleHori horiOp -> do
        (ctl, csl, ctm, csr, ctr) <- except $ applyHori horiOp tl sm tr
        nextSteps <- go sl (Path ctl csl (Path ctm csr (mkrest ctr))) False
        pure $ LMHorizontalize horiOp : nextSteps

obsDerivation
  :: [PVLeftmost SPC]
  -> Path (Edges SPC) (Notes SPC)
  -> Either String (Trace PVParams)
obsDerivation deriv top = execStateT (go Start top False deriv) (Trace mempty)
 where
  go
    :: StartStop (Notes SPC)
    -> Path (Edges SPC) (Notes SPC)
    -> Bool
    -> [PVLeftmost SPC]
    -> PVObs ()
  go _sl _surface        _ars []          = lift $ Left "Derivation incomplete."
  go sl  (PathEnd trans) _ars (op : rest) = case op of
    LMSingle single -> do
      obsSingleStep (sl, trans, Stop) single
      case single of
        LMSingleFreeze _       -> pure ()
        LMSingleSplit  splitOp -> do
          (ctl, cs, ctr) <- lift $ applySplit splitOp trans
          go sl (Path ctl cs (PathEnd ctr)) False rest
    LMDouble _ -> lift $ Left "Double operation on single transition."
  go sl (Path tl sm (PathEnd tr)) ars (op : rest) =
    goDouble op rest ars (sl, tl, sm, tr, Stop) PathEnd
  go sl (Path tl sm (Path tr sr pathRest)) ars (op : derivRest) =
    goDouble op derivRest ars (sl, tl, sm, tr, Inner sr)
      $ \tr' -> Path tr' sr pathRest

  goDouble op rest ars (sl, tl, sm, tr, sr) mkRest = case op of
    LMSingle _ -> lift $ Left "Single operation with several transitions left."
    LMDouble double -> do
      obsDoubleStep (sl, tl, sm, tr, sr) ars double
      case double of
        LMDoubleFreezeLeft _ -> do
          when ars $ lift $ Left "FreezeLeft after SplitRight."
          go (Inner sm) (mkRest tr) False rest
        LMDoubleSplitLeft splitOp -> do
          when ars $ lift $ Left "SplitLeft after SplitRight."
          (ctl, cs, ctr) <- lift $ applySplit splitOp tl
          go sl (Path ctl cs $ Path ctr sm $ mkRest tr) False rest
        LMDoubleSplitRight splitOp -> do
          (ctl, cs, ctr) <- lift $ applySplit splitOp tr
          go sl (Path tl sm $ Path ctl cs $ mkRest ctr) True rest
        LMDoubleHori horiOp -> do
          (ctl, csl, ctm, csr, ctr) <- lift $ applyHori horiOp tl sm tr
          go sl (Path ctl csl $ Path ctm csr $ mkRest ctr) False rest

sampleSingleStep
  :: _ => ContextSingle SPC -> m (LeftmostSingle (Split SPC) Freeze)
sampleSingleStep parents@(_, trans, _) = if freezable trans
  then do
    shouldFreeze <- sampleValue Bernoulli $ pOuter . pSingleFreeze
    if shouldFreeze
      then LMSingleFreeze <$> sampleFreeze parents
      else LMSingleSplit <$> sampleSplit parents
  else LMSingleSplit <$> sampleSplit parents

obsSingleStep
  :: ContextSingle SPC -> LeftmostSingle (Split SPC) Freeze -> PVObs ()
obsSingleStep parents@(_, trans, _) singleOp = if freezable trans
  then case singleOp of
    LMSingleFreeze f -> do
      observeValue Bernoulli (pOuter . pSingleFreeze) True
      observeFreeze parents f
    LMSingleSplit s -> do
      observeValue Bernoulli (pOuter . pSingleFreeze) False
      observeSplit parents s
  else case singleOp of
    LMSingleFreeze _ -> lift $ Left "Freezing a non-freezable transition."
    LMSingleSplit  s -> observeSplit parents s

sampleDoubleStep
  :: _
  => ContextDouble SPC
  -> Bool
  -> m (LeftmostDouble (Split SPC) Freeze (Hori SPC))
sampleDoubleStep parents@(sliceL, transL, sliceM, transR, sliceR) afterRightSplit
  = if afterRightSplit
    then do
      shouldSplitRight <- sampleValue Bernoulli $ pOuter . pDoubleRightSplit
      if shouldSplitRight
        then LMDoubleSplitRight <$> sampleSplit (Inner sliceM, transR, sliceR)
        else LMDoubleHori <$> sampleHori parents
    else do
      continueLeft <- sampleValue Bernoulli $ pOuter . pDoubleLeft
      if continueLeft
        then if freezable transL
          then do
            shouldFreeze <- sampleValue Bernoulli $ pOuter . pDoubleLeftFreeze
            if shouldFreeze
              then LMDoubleSplitLeft
                <$> sampleSplit (sliceL, transL, Inner sliceM)
              else LMDoubleFreezeLeft
                <$> sampleFreeze (sliceL, transL, Inner sliceM)
          else LMDoubleSplitLeft <$> sampleSplit (sliceL, transL, Inner sliceM)
        else sampleDoubleStep parents True

obsDoubleStep
  :: ContextDouble SPC
  -> Bool
  -> LeftmostDouble (Split SPC) Freeze (Hori SPC)
  -> PVObs ()
obsDoubleStep parents@(sliceL, transL, sliceM, transR, sliceR) afterRightSplit doubleOp
  = case doubleOp of
    LMDoubleFreezeLeft f -> do
      observeValue Bernoulli (pOuter . pDoubleLeft)       True
      observeValue Bernoulli (pOuter . pDoubleLeftFreeze) True
      observeFreeze (sliceL, transL, Inner sliceM) f
    LMDoubleSplitLeft s -> do
      observeValue Bernoulli (pOuter . pDoubleLeft)       True
      observeValue Bernoulli (pOuter . pDoubleLeftFreeze) False
      observeSplit (sliceL, transL, Inner sliceM) s
    LMDoubleSplitRight s -> do
      unless afterRightSplit
        $ observeValue Bernoulli (pOuter . pDoubleLeft) False
      observeValue Bernoulli (pOuter . pDoubleRightSplit) True
      observeSplit (Inner sliceM, transR, sliceR) s
    LMDoubleHori h -> do
      unless afterRightSplit
        $ observeValue Bernoulli (pOuter . pDoubleLeft) False
      observeValue Bernoulli (pOuter . pDoubleRightSplit) False
      observeHori parents h

sampleFreeze :: RandomInterpreter m PVParams => ContextSingle n -> m Freeze
sampleFreeze _parents = pure FreezeOp

observeFreeze :: ContextSingle SPC -> Freeze -> PVObs ()
observeFreeze _parents FreezeOp = pure ()

-- helper for sampleSplit and observeSplit
collectElabos
  :: [(Edge SPC, [(SPC, o1)])]
  -> [(InnerEdge SPC, [(SPC, Passing)])]
  -> [(SPC, [(SPC, o2)])]
  -> [(SPC, [(SPC, o3)])]
  -> ( M.Map (StartStop SPC, StartStop SPC) [(SPC, o1)]
     , M.Map (SPC, SPC) [(SPC, Passing)]
     , M.Map SPC [(SPC, o2)]
     , M.Map SPC [(SPC, o3)]
     , S.HashSet (Edge SPC)
     , S.HashSet (Edge SPC)
     )
collectElabos childrenT childrenNT childrenL childrenR =
  let splitTs    = M.fromList childrenT
      splitNTs   = M.fromList childrenNT
      fromLeft   = M.fromList childrenL
      fromRight  = M.fromList childrenR
      keepLeftT  = getEdges childrenT (\p m -> (fst p, Inner m))
      keepLeftL  = getEdges childrenL (\l m -> (Inner l, Inner m))
      keepLeftNT = do -- List
        ((l, _), cs ) <- childrenNT
        (m     , orn) <- cs
        guard $ orn == PassingRight
        pure (Inner l, Inner m)
      leftEdges   = S.fromList $ keepLeftT <> keepLeftNT <> keepLeftL
      keepRightT  = getEdges childrenT (\p m -> (Inner m, snd p))
      keepRightR  = getEdges childrenR (\r m -> (Inner m, Inner r))
      keepRightNT = do -- List
        ((_, r), cs ) <- childrenNT
        (m     , orn) <- cs
        guard $ orn == PassingLeft
        pure (Inner m, Inner r)
      rightEdges = S.fromList $ keepRightT <> keepRightNT <> keepRightR
  in  (splitTs, splitNTs, fromLeft, fromRight, leftEdges, rightEdges)
 where
  getEdges :: [(p, [(c, o)])] -> (p -> c -> Edge SPC) -> [Edge SPC]
  getEdges elabos mkEdge = do -- List
    (p, cs) <- elabos
    (c, _ ) <- cs
    pure $ mkEdge p c

-- helper for sampleSplit and observeSplit
collectNotes
  :: [(Edge SPC, [(SPC, o1)])]
  -> [(InnerEdge SPC, [(SPC, Passing)])]
  -> [(SPC, [(SPC, o2)])]
  -> [(SPC, [(SPC, o3)])]
  -> [SPC]
collectNotes childrenT childrenNT childrenL childrenR =
  let notesT     = concatMap (fmap fst . snd) childrenT
      notesNT    = concatMap (fmap fst . snd) childrenNT
      notesFromL = concatMap (fmap fst . snd) childrenL
      notesFromR = concatMap (fmap fst . snd) childrenR
  in  notesT <> notesNT <> notesFromL <> notesFromR

sampleSplit :: forall m . _ => ContextSingle SPC -> m (Split SPC)
sampleSplit (sliceL, Edges ts nts, sliceR) = do
  -- ornament regular edges at least once
  childrenT  <- mapM sampleT $ S.toList ts
  -- ornament passing edges exactly once
  childrenNT <- mapM sampleNT $ MS.toOccurList nts
  -- ornament left notes
  childrenL  <- case getInner sliceL of
    Nothing            -> pure []
    Just (Notes notes) -> mapM sampleL $ MS.toList notes
  -- ornament right notes
  childrenR <- case getInner sliceR of
    Nothing            -> pure []
    Just (Notes notes) -> mapM sampleR $ MS.toList notes
  -- introduce new passing edges left and right
  let notes = collectNotes childrenT childrenNT childrenL childrenR
  passLeft <- case getInner sliceL of
    Nothing -> pure MS.empty
    Just (Notes notesl) ->
      samplePassing (MS.toList notesl) notes pNewPassingLeft
  passRight <- case getInner sliceR of
    Nothing -> pure MS.empty
    Just (Notes notesr) ->
      samplePassing notes (MS.toList notesr) pNewPassingRight
  let (splitTs, splitNTs, fromLeft, fromRight, leftEdges, rightEdges) =
        collectElabos childrenT childrenNT childrenL childrenR
  -- decide which edges to keep
  keepLeft  <- sampleKeepEdges pKeepL leftEdges
  keepRight <- sampleKeepEdges pKeepR rightEdges
  -- combine all sampling results into split operation
  pure $ SplitOp { splitTs
                 , splitNTs
                 , fromLeft
                 , fromRight
                 , keepLeft
                 , keepRight
                 , passLeft
                 , passRight
                 }

observeSplit :: ContextSingle SPC -> Split SPC -> PVObs ()
observeSplit (sliceL, Edges ts nts, sliceR) (SplitOp splitTs splitNTs fromLeft fromRight keepLeft keepRight passLeft passRight)
  = do
  -- observe ornaments of regular edges
    childrenT  <- mapM (observeT splitTs) $ S.toList ts
    -- observe ornaments of passing edges
    childrenNT <- mapM (observeNT splitNTs) $ MS.toOccurList nts
    -- observe ornaments of left notes
    childrenL  <- case getInner sliceL of
      Nothing            -> pure []
      Just (Notes notes) -> mapM (observeL fromLeft) $ MS.toList notes
    -- observe ornaments of right notes
    childrenR <- case getInner sliceR of
      Nothing            -> pure []
      Just (Notes notes) -> mapM (observeR fromRight) $ MS.toList notes
    -- observe new passing edges
    let notes = collectNotes childrenT childrenNT childrenL childrenR
    case getInner sliceL of
      Nothing -> pure ()
      Just (Notes notesl) ->
        observePassing (MS.toList notesl) notes pNewPassingLeft passLeft
    case getInner sliceR of
      Nothing -> pure ()
      Just (Notes notesr) ->
        observePassing notes (MS.toList notesr) pNewPassingRight passRight
    -- observe which edges are kept
    let (_, _, _, _, leftEdges, rightEdges) =
          collectElabos childrenT childrenNT childrenL childrenR
    observeKeepEdges pKeepL leftEdges  keepLeft
    observeKeepEdges pKeepR rightEdges keepRight

sampleRootNote :: _ => m SPC
sampleRootNote = do
  fifthsSign <- sampleConst Bernoulli 0.5
  fifthsN    <- sampleValue Geometric0 $ pInner . pRootFifths
  let interval = if fifthsSign then fifthsN else negate fifthsN
  pure $ spc interval

observeRootNote :: SPC -> PVObs ()
observeRootNote child = do
  observeConst Bernoulli 0.5 fifthsSign
  observeValue Geometric0 (pInner . pRootFifths) fifthsN
 where
  fs         = fifths child
  fifthsSign = fs >= 0
  fifthsN    = abs fs

sampleNeighbor :: _ => Bool -> SPC -> m SPC
sampleNeighbor stepUp ref = do
  chromatic <- sampleValue Bernoulli $ pInner . pNBChromatic
  if chromatic
    then do
      alt <- sampleValue Geometric0 $ pInner . pNBAlt
      let altInterval = alt *^ chromaticSemitone
      pure $ ref +^ if stepUp then altInterval else down altInterval
    else do
      alt   <- sampleValue Geometric0 $ pInner . pNBAlt
      altUp <- sampleConst Bernoulli 0.5
      let altInterval = alt *^ chromaticSemitone
          step        = if altUp == stepUp
            then major second' ^+^ altInterval
            else minor second' ^-^ altInterval
      pure $ ref +^ if stepUp then step else down step

observeNeighbor :: Bool -> SPC -> SPC -> PVObs ()
observeNeighbor goesUp ref nb = do
  let interval    = ref `pto` nb
      isChromatic = direction interval == EQ
  observeValue Bernoulli (pInner . pNBChromatic) isChromatic
  if isChromatic
    then do
      let alt = abs (alteration interval)
      observeValue Geometric0 (pInner . pNBAlt) alt
    else do
      let alt   = alteration (iabs interval)
          altUp = (alt >= 0) == goesUp
          altN  = if alt >= 0 then alt else (-alt) - 1
      observeValue Geometric0 (pInner . pNBAlt) altN
      observeConst Bernoulli 0.5 altUp

sampleDoubleChild :: _ => SPC -> SPC -> m (SPC, DoubleOrnament)
sampleDoubleChild pl pr
  | pl == pr = do
    rep <- sampleValue Bernoulli $ pInner . pRepeatOverNeighbor
    if rep
      then pure (pl, FullRepeat)
      else do
        stepUp <- sampleConst Bernoulli 0.5
        (, FullNeighbor) <$> sampleNeighbor stepUp pl
  | otherwise = do
    repeatLeft <- sampleValue Bernoulli $ pInner . pRepeatLeftOverRight
    if repeatLeft
      then pure (pl, RightRepeatOfLeft)
      else pure (pr, LeftRepeatOfRight)

observeDoubleChild :: SPC -> SPC -> SPC -> PVObs ()
observeDoubleChild pl pr child
  | pl == pr = do
    let isRep = child == pl
    observeValue Bernoulli (pInner . pRepeatOverNeighbor) isRep
    unless isRep $ do
      let dir    = direction (pl `pto` child)
      let goesUp = dir == GT || (dir == EQ && alteration child > alteration pl)
      observeConst Bernoulli 0.5 goesUp
      observeNeighbor goesUp pl child
  | otherwise = observeValue Bernoulli
                             (pInner . pRepeatLeftOverRight)
                             (pl == child)

sampleT :: _ => Edge SPC -> m (Edge SPC, [(SPC, DoubleOrnament)])
sampleT (l, r) = do
  n        <- sampleValue Geometric1 $ pInner . pElaborateRegular
  children <- permutationPlate n $ case (l, r) of
    (Start, Stop) -> do
      child <- sampleRootNote
      pure $ Just (child, RootNote)
    (Inner pl, Inner pr) -> do
      (child, orn) <- sampleDoubleChild pl pr
      pure $ Just (child, orn)
    _ -> pure Nothing
  pure ((l, r), catMaybes children)

observeT
  :: M.Map (Edge SPC) [(SPC, DoubleOrnament)]
  -> Edge SPC
  -> PVObs (Edge SPC, [(SPC, DoubleOrnament)])
observeT splitTs parents = do
  let children = fromMaybe [] $ M.lookup parents splitTs
  forM_ children $ \(child, _) -> case parents of
    (Start, Stop) -> do
      observeRootNote child
    (Inner pl, Inner pr) -> do
      observeDoubleChild pl pr child
    _ -> lift $ Left $ "Invalid parent edge " <> show parents <> "."
  pure (parents, children)

-- requires distance >= M2
sampleChromPassing :: _ => Pitch a -> Pitch a -> m (Pitch a, Passing)
sampleChromPassing pl pr = do
  atLeft <- sampleValue Bernoulli $ pInner . pConnectChromaticLeftOverRight
  let dir   = if direction (pl `pto` pr) == GT then id else down
      child = if atLeft
        then pl +^ dir chromaticSemitone
        else pr -^ dir chromaticSemitone
  pure (child, PassingMid)

observeChromPassing :: SPC -> SPC -> SPC -> PVObs ()
observeChromPassing pl _pr child = observeValue
  Bernoulli
  (pInner . pConnectChromaticLeftOverRight)
  (degree pl == degree child)

sampleMidPassing :: _ => Pitch SIC -> Pitch SIC -> m (SPC, Passing)
sampleMidPassing pl pr = do
  child <- sampleNeighbor (direction (pl `pto` pr) == GT) pl
  pure (child, PassingMid)

observeMidPassing :: SPC -> SPC -> SPC -> PVObs ()
observeMidPassing pl pr = observeNeighbor (direction (pl `pto` pr) == GT) pl

sampleNonMidPassing :: _ => Pitch SIC -> Pitch SIC -> m (SPC, Passing)
sampleNonMidPassing pl pr = do
  left <- sampleValue Bernoulli $ pInner . pPassLeftOverRight
  let dirUp = direction (pl `pto` pr) == GT
  if left
    then do
      child <- sampleNeighbor dirUp pl
      pure (child, PassingLeft)
    else do
      child <- sampleNeighbor (not dirUp) pr
      pure (child, PassingRight)

observeNonMidPassing :: SPC -> SPC -> SPC -> Passing -> PVObs ()
observeNonMidPassing pl pr child orn = do
  let left  = orn == PassingLeft
      dirUp = direction (pl `pto` pr) == GT
  observeValue Bernoulli (pInner . pPassLeftOverRight) left
  if left
    then observeNeighbor dirUp pl child
    else observeNeighbor (not dirUp) pr child

sampleNT :: _ => (InnerEdge SPC, Int) -> m (InnerEdge SPC, [(SPC, Passing)])
sampleNT ((pl, pr), n) = do
  children <- permutationPlate n $ case degree $ iabs $ pl `pto` pr of
    1 -> sampleChromPassing pl pr
    2 -> do
      connect <- sampleValue Bernoulli $ pInner . pConnect
      if connect then sampleMidPassing pl pr else sampleNonMidPassing pl pr
    _ -> sampleNonMidPassing pl pr
  pure ((pl, pr), children)

observeNT
  :: _
  => M.Map (InnerEdge SPC) [(SPC, Passing)]
  -> (InnerEdge SPC, Int)
  -> PVObs (InnerEdge SPC, [(SPC, Passing)])
observeNT splitNTs ((pl, pr), _n) = do
  let children = fromMaybe [] $ M.lookup (pl, pr) splitNTs
  forM_ children $ \(child, orn) -> case degree $ iabs $ pl `pto` pr of
    1 -> observeChromPassing pl pr child
    2 -> case orn of
      PassingMid -> observeMidPassing pl pr child
      _          -> observeNonMidPassing pl pr child orn
    _ -> observeNonMidPassing pl pr child orn
  pure ((pl, pr), children)

sampleSingleOrn
  :: _ => SPC -> o -> o -> Accessor PVParamsInner Beta -> m (SPC, [(SPC, o)])
sampleSingleOrn parent oRepeat oNeighbor pElaborate = do
  n        <- sampleValue Geometric0 $ pInner . pElaborate
  children <- permutationPlate n $ do
    rep <- sampleValue Bernoulli $ pInner . pRepeatOverNeighbor
    if rep
      then pure (parent, oRepeat)
      else do
        stepUp <- sampleConst Bernoulli 0.5
        child  <- sampleNeighbor stepUp parent
        pure (child, oNeighbor)
  pure (parent, children)

observeSingleOrn
  :: M.Map SPC [(SPC, o)]
  -> SPC
  -> Accessor PVParamsInner Beta
  -> PVObs (SPC, [(SPC, o)])
observeSingleOrn table parent pElaborate = do
  let children = fromMaybe [] $ M.lookup parent table
  observeValue Geometric0 (pInner . pElaborate) (length children)
  forM_ children $ \(child, _) -> do
    let rep = child == parent
    observeValue Bernoulli (pInner . pRepeatOverNeighbor) rep
    unless rep $ do
      let dir = direction (child `pto` parent)
          up  = dir == GT || (dir == EQ && alteration child > alteration parent)
      observeConst Bernoulli 0.5 up
      observeNeighbor up parent child
  pure (parent, children)

sampleL :: _ => SPC -> m (SPC, [(SPC, RightOrnament)])
sampleL parent = sampleSingleOrn parent RightRepeat RightNeighbor pElaborateL

observeL
  :: M.Map SPC [(SPC, RightOrnament)]
  -> SPC
  -> PVObs (SPC, [(SPC, RightOrnament)])
observeL ls parent = observeSingleOrn ls parent pElaborateL

sampleR :: _ => SPC -> m (SPC, [(SPC, LeftOrnament)])
sampleR parent = sampleSingleOrn parent LeftRepeat LeftNeighbor pElaborateR

observeR
  :: M.Map SPC [(SPC, LeftOrnament)]
  -> SPC
  -> PVObs (SPC, [(SPC, LeftOrnament)])
observeR rs parent = observeSingleOrn rs parent pElaborateR

sampleKeepEdges
  :: _ => Accessor PVParamsInner Beta -> S.HashSet e -> m (S.HashSet e)
sampleKeepEdges pKeep set = do
  kept <- mapM sKeep (S.toList set)
  pure $ S.fromList $ catMaybes kept
 where
  sKeep elt = do
    keep <- sampleValue Bernoulli (pInner . pKeep)
    pure $ if keep then Just elt else Nothing

observeKeepEdges
  :: (Eq e, Hashable e)
  => Accessor PVParamsInner Beta
  -> S.HashSet e
  -> S.HashSet e
  -> PVObs ()
observeKeepEdges pKeep candidates kept = mapM_ oKeep (S.toList candidates)
 where
  oKeep edge = observeValue Bernoulli (pInner . pKeep) (S.member edge kept)

sampleHori :: _ => ContextDouble SPC -> m (Hori SPC)
sampleHori (_sliceL, _transL, Notes sliceM, _transR, _sliceR) = do
  -- distribute notes
  dists <- mapM distNote $ MS.toOccurList sliceM
  let notesLeft = catMaybes $ flip fmap dists $ \((note, n), to) -> case to of
        ToRight dl -> if n - dl > 0 then Just note else Nothing
        _          -> Just note
      notesRight = catMaybes $ flip fmap dists $ \((note, n), to) -> case to of
        ToLeft dr -> if n - dr > 0 then Just note else Nothing
        _         -> Just note
  -- generate repetition edges
  repeats <- sequence $ do -- List
    l <- notesLeft
    r <- notesRight
    guard $ l == r
    pure $ do -- m
      rep <- sampleValue Bernoulli $ pInner . pHoriRepetitionEdge
      pure $ if rep then Just (Inner l, Inner r) else Nothing
  let repEdges = S.fromList $ catMaybes repeats
  -- generate passing edges
  passEdges <- samplePassing notesLeft notesRight pNewPassingMid
  -- construct result
  let distMap = HM.fromList (Bi.first fst <$> dists)
      edges   = Edges repEdges passEdges
  pure $ HoriOp distMap edges
 where
  -- distribute a note to the two child slices
  distNote (note, n) = do
    dir <- sampleValue (Categorical @3) $ pInner . pNoteHoriDirection
    to  <- case dir of
      0 -> pure ToBoth
      1 -> do
        nother <- sampleValue (Binomial $ n - 1) $ pInner . pNotesOnOtherSide
        pure $ ToLeft $ n - nother
      _ -> do
        nother <- sampleValue (Binomial $ n - 1) $ pInner . pNotesOnOtherSide
        pure $ ToRight $ n - nother
    pure ((note, n), to)

observeHori :: ContextDouble SPC -> Hori SPC -> PVObs ()
observeHori (_sliceL, _transL, Notes sliceM, _transR, _sliceR) (HoriOp obsDists (Edges repEdges passEdges))
  = do
    -- observe note distribution
    dists <- mapM (observeNoteDist obsDists) $ MS.toOccurList sliceM
    let notesLeft = catMaybes $ flip fmap dists $ \((note, n), to) ->
          case to of
            ToRight dl -> if n - dl > 0 then Just note else Nothing
            _          -> Just note
        notesRight = catMaybes $ flip fmap dists $ \((note, n), to) ->
          case to of
            ToLeft dr -> if n - dr > 0 then Just note else Nothing
            _         -> Just note
    -- observe repetition edges
    sequence_ $ do -- List
      l <- notesLeft
      r <- notesRight
      guard $ l == r
      pure $ observeValue Bernoulli
                          (pInner . pHoriRepetitionEdge)
                          (S.member (Inner l, Inner r) repEdges)
    -- observe passing edges
    observePassing notesLeft notesRight pNewPassingMid passEdges
 where
  observeNoteDist distMap (parent, n) = case HM.lookup parent distMap of
    Nothing ->
      lift $ Left $ "Note " <> showNotation parent <> " is not distributed."
    Just dir -> do
      case dir of
        ToBoth -> do
          observeValue (Categorical @3) (pInner . pNoteHoriDirection) 0
        ToLeft ndiff -> do
          observeValue (Categorical @3) (pInner . pNoteHoriDirection) 1
          observeValue (Binomial $ n - 1)
                       (pInner . pNotesOnOtherSide)
                       (n - ndiff)
        ToRight ndiff -> do
          observeValue (Categorical @3) (pInner . pNoteHoriDirection) 2
          observeValue (Binomial $ n - 1)
                       (pInner . pNotesOnOtherSide)
                       (n - ndiff)
      pure ((parent, n), dir)

samplePassing
  :: _
  => [SPC]
  -> [SPC]
  -> Accessor PVParamsInner Beta
  -> m (MS.MultiSet (InnerEdge SPC))
samplePassing notesLeft notesRight pNewPassing =
  fmap (MS.fromList . concat) $ sequence $ do -- List
    l <- notesLeft
    r <- notesRight
    guard $ iabs (l `pto` r) <= major second'
    pure $ do -- m
      n <- sampleValue Geometric0 $ pInner . pNewPassing
      pure $ replicate n (l, r)

observePassing
  :: [SPC]
  -> [SPC]
  -> Accessor PVParamsInner Beta
  -> MS.MultiSet (InnerEdge SPC)
  -> PVObs ()
observePassing notesLeft notesRight pNewPassing edges = sequence_ $ do
  l <- notesLeft
  r <- notesRight
  guard $ iabs (l `pto` r) <= major second'
  pure $ observeValue Geometric0 (pInner . pNewPassing) (edges MS.! (l, r))
