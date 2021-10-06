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
                                                , replicateM
                                                , unless
                                                , when
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State      ( StateT
                                                , execStateT
                                                )
import qualified Data.Bifunctor                as Bi
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as S
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( catMaybes )
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
  :: [((StartStop SPC, StartStop SPC), [(SPC, o1)])]
  -> [((SPC, SPC), (SPC, Passing))]
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
      splitNTs   = M.fromListWith (<>) $ Bi.second (: []) <$> childrenNT
      fromLeft   = M.fromList childrenL
      fromRight  = M.fromList childrenR
      keepLeftT  = getEdges childrenT (\p m -> (fst p, Inner m))
      keepLeftL  = getEdges childrenL (\l m -> (Inner l, Inner m))
      keepLeftNT = do -- List
        ((l, _), (m, orn)) <- childrenNT
        guard $ orn == PassingRight
        pure (Inner l, Inner m)
      leftEdges   = S.fromList $ keepLeftT <> keepLeftNT <> keepLeftL
      keepRightT  = getEdges childrenT (\p m -> (Inner m, snd p))
      keepRightR  = getEdges childrenR (\r m -> (Inner m, Inner r))
      keepRightNT = do -- List
        ((_, r), (m, orn)) <- childrenNT
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
  :: [((StartStop SPC, StartStop SPC), [(SPC, o1)])]
  -> [((SPC, SPC), (SPC, Passing))]
  -> [(SPC, [(SPC, o2)])]
  -> [(SPC, [(SPC, o3)])]
  -> [SPC]
collectNotes childrenT childrenNT childrenL childrenR =
  let notesT     = concatMap (fmap fst . snd) childrenT
      notesNT    = fmap (fst . snd) childrenNT
      notesFromL = concatMap (fmap fst . snd) childrenL
      notesFromR = concatMap (fmap fst . snd) childrenR
  in  notesT <> notesNT <> notesFromL <> notesFromR

sampleSplit :: forall m . _ => ContextSingle SPC -> m (Split SPC)
sampleSplit (sliceL, Edges ts nts, sliceR) = do
  -- ornament regular edges at least once
  childrenT  <- mapM sampleT $ S.toList ts
  -- ornament passing edges exactly once
  childrenNT <- mapM sampleNT $ MS.toList nts
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
    childrenNT <- mapM (observeNT splitNTs) $ MS.toList nts
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

sampleNeighbor :: _ => Bool -> SPC -> m SPC
sampleNeighbor stepUp ref = do
  chromatic <- sampleValue Bernoulli $ pInner . pNBChromatic
  -- 
  altUp     <- sampleConst Bernoulli 0.5
  alt       <- sampleValue Geometric0 $ pInner . pNBAlt
  let altInterval = alt *^ chromaticSemitone
  pure $ ref +^ if chromatic
    then (if altUp then id else down) altInterval
    else (if stepUp then id else down) $ if altUp == stepUp
      then major second' ^+^ altInterval
      else minor second' ^-^ altInterval

sampleRootNote :: _ => m SPC
sampleRootNote = do
  fifthsSign <- sampleConst Bernoulli 0.5
  fifthsN    <- sampleValue Geometric0 $ pInner . pRootFifths
  let interval = if fifthsSign then fifthsN else negate fifthsN
  pure $ spc interval

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

sampleT :: _ => Edge SPC -> m (Edge SPC, [(SPC, DoubleOrnament)])
sampleT (l, r) = do
  n <- sampleValue Geometric1 $ pInner . pElaborateRegular
  fmap (((l, r), ) . catMaybes) $ replicateM n $ case (l, r) of
    (Start, Stop) -> do
      child <- sampleRootNote
      pure $ Just (child, RootNote)
    (Inner pl, Inner pr) -> do
      (child, orn) <- sampleDoubleChild pl pr
      -- keepLeft     <- sampleValue Bernoulli $ pInner . pKeepL
      -- keepRight    <- sampleValue Bernoulli $ pInner . pKeepR
      pure $ Just (child, orn)
    _ -> pure Nothing

observeT
  :: M.Map (Edge SPC) [(n, DoubleOrnament)]
  -> Edge SPC
  -> PVObs (Edge SPC, [(SPC, DoubleOrnament)])
observeT splitTs (l, r) = do
  pure undefined

-- requires distance >= M2
sampleChromPassing :: _ => Pitch a -> Pitch a -> m (Pitch a, Passing)
sampleChromPassing pl pr = do
  atLeft <- sampleValue Bernoulli $ pInner . pConnectChromaticLeftOverRight
  let dir   = if direction (pl `pto` pr) == GT then id else down
      child = if atLeft
        then pl +^ dir chromaticSemitone
        else pr -^ dir chromaticSemitone
  pure (child, PassingMid)

sampleMidPassing :: _ => Pitch SIC -> Pitch SIC -> m (SPC, Passing)
sampleMidPassing pl pr = do
  child <- sampleNeighbor (direction (pl `pto` pr) == GT) pl
  pure (child, PassingMid)

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

sampleNT :: _ => InnerEdge SPC -> m (InnerEdge SPC, (SPC, Passing))
sampleNT (pl, pr) = fmap ((pl, pr), ) $ case degree $ iabs $ pl `pto` pr of
  1 -> sampleChromPassing pl pr
  2 -> do
    connect <- sampleValue Bernoulli $ pInner . pConnect
    if connect then sampleMidPassing pl pr else sampleNonMidPassing pl pr
  _ -> sampleNonMidPassing pl pr

observeNT
  :: _
  => M.Map (InnerEdge SPC) [(SPC, Passing)]
  -> InnerEdge SPC
  -> PVObs (InnerEdge SPC, (SPC, Passing))
observeNT splitNTs (pl, pr) = undefined

sampleL :: _ => SPC -> m (SPC, [(SPC, RightOrnament)])
sampleL parent = do
  n <- sampleValue Geometric0 $ pInner . pElaborateL
  fmap (parent, ) $ replicateM n $ do
    rep <- sampleValue Bernoulli $ pInner . pRepeatOverNeighbor
    if rep
      then pure (parent, RightRepeat)
      else do
        stepUp <- sampleConst Bernoulli 0.5
        child  <- sampleNeighbor stepUp parent
        pure (child, RightNeighbor)

observeL
  :: M.Map SPC [(SPC, RightOrnament)]
  -> SPC
  -> PVObs (SPC, [(SPC, RightOrnament)])
observeL = error "not implemented"

sampleR :: _ => SPC -> m (SPC, [(SPC, LeftOrnament)])
sampleR parent = do
  n <- sampleValue Geometric0 $ pInner . pElaborateR
  fmap (parent, ) $ replicateM n $ do
    rep <- sampleValue Bernoulli $ pInner . pRepeatOverNeighbor
    if rep
      then pure (parent, LeftRepeat)
      else do
        stepUp <- sampleConst Bernoulli 0.5
        child  <- sampleNeighbor stepUp parent
        pure (child, LeftNeighbor)

observeR
  :: M.Map SPC [(SPC, LeftOrnament)]
  -> SPC
  -> PVObs (SPC, [(SPC, LeftOrnament)])
observeR = error "not implemented"

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
  :: Accessor PVParamsInner Beta -> S.HashSet e -> S.HashSet e -> PVObs ()
observeKeepEdges pKeep candidates kept = undefined

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
