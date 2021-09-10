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
import           Inference.Conjugate
import           PVGrammar
import           PVGrammar.Generate

import           Control.Monad                  ( replicateM
                                                , guard
                                                )
import qualified Data.Bifunctor                as Bi
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as S
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( catMaybes
                                                , mapMaybe
                                                )
import qualified Internal.MultiSet             as MS
import           Musicology.Pitch              as MP
import           Lens.Micro.TH                  ( makeLenses )
import           GHC.Generics                   ( Generic )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class      ( lift )

data PVParamsOuter f = PVParamsOuter
  { _pSingleFreeze :: f Beta
  , _pDoubleLeft :: f Beta
  , _pDoubleLeftFreeze :: f Beta
  , _pDoubleRightSplit :: f Beta
  }
  deriving (Generic)

deriving instance (Show (f Beta)) => Show (PVParamsOuter f)

makeLenses ''PVParamsOuter

data PVParamsInner f = PVParamsInner
  -- split
  { _pElaborateRegular :: f Beta
  , _pElaborateL :: f Beta
  , _pElaborateR :: f Beta
  , _pRootFifths :: f Beta
  , _pKeepL :: f Beta
  , _pKeepR :: f Beta
  , _pRepeatOverNeighbor :: f Beta
  , _pNBChromatic :: f Beta
  , _pNBAlt :: f Beta
  , _pRepeatLeftOverRight :: f Beta
  , _pConnect :: f Beta
  , _pConnectChromaticLeftOverRight :: f Beta
  , _pPassLeftOverRight :: f Beta
  , _pNewPassingLeft :: f Beta
  , _pNewPassingRight :: f Beta
  -- hori
  , _pNewPassingMid :: f Beta
  , _pNoteHoriDirection :: f (Dirichlet 3)
  , _pNotesOnOtherSide :: f Beta
  , _pHoriRepetitionEdge :: f Beta
  }
  deriving (Generic)

deriving instance ( Show (f Beta)
                  , Show (f Beta)
                  , Show (f Beta)
                  , Show (f (Dirichlet 3))
                  , Show (f Beta)
                  ) => Show (PVParamsInner f)

makeLenses ''PVParamsInner

data PVParams f = PVParams { _pOuter :: PVParamsOuter f
                           , _pInner :: PVParamsInner f }
  deriving (Generic)

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

sampleDerivation'
  :: _ => m (Either String [Leftmost (Split SPC) Freeze (Hori SPC)])
sampleDerivation' =
  sampleDerivation $ PathEnd (Edges (S.singleton ((:⋊), (:⋉))) MS.empty)

sampleDerivation
  :: _
  => Path (Edges SPC) (Notes SPC)
  -> m (Either String [Leftmost (Split SPC) Freeze (Hori SPC)])
sampleDerivation top = runExceptT $ go (:⋊) top False
 where
  go sl surface ars = case surface of
    -- 1 trans left:
    PathEnd t -> do
      step <- lift $ sampleSingleStep (sl, t, (:⋉))
      case step of
        LMSingleSplit splitOp -> do
          (ctl, cs, ctr) <- except $ applySplit splitOp t
          nextSteps      <- go sl (Path ctl cs (PathEnd ctr)) False
          pure $ LMSplitOnly splitOp : nextSteps
        LMSingleFreeze freezeOp -> pure [LMFreezeOnly freezeOp]
    -- 2 transs left
    Path tl sm (PathEnd tr) -> goDouble sl tl sm tr (:⋉) ars PathEnd
    -- 3 or more transs left
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

sampleSingleStep
  :: _ => ContextSingle SPC -> m (LeftmostSingle (Split SPC) Freeze)
sampleSingleStep parents@(_, trans, _) = if freezable trans
  then do
    shouldFreeze <- sampleValue Bernoulli $ pOuter . pSingleFreeze
    if shouldFreeze
      then LMSingleFreeze <$> sampleFreeze parents
      else LMSingleSplit <$> sampleSplit parents
  else LMSingleSplit <$> sampleSplit parents

sampleDoubleStep
  :: _
  => ContextDouble SPC
  -> Bool
  -> m (LeftmostDouble (Split SPC) Freeze (Hori SPC))
sampleDoubleStep parents@(sliceL, transL, sliceM, transR, sliceR) afterSplitRight
  = if afterSplitRight
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
        else sampleDoubleStep parents False

sampleFreeze :: RandomInterpreter m PVParams => ContextSingle n -> m Freeze
sampleFreeze _parent = pure FreezeOp

sampleSplit :: forall  m . _ => ContextSingle SPC -> m (Split SPC)
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
  let notesT     = concatMap (\(_, ns) -> fst . fst <$> ns) childrenT
      notesNT    = fmap (fst . fst . snd) childrenNT
      notesFromL = concatMap (\(_, ns) -> fmap (\(n, _, _) -> n) ns) childrenL
      notesFromR = concatMap (\(_, ns) -> fmap (\(n, _, _) -> n) ns) childrenR
      notes      = notesT <> notesNT <> notesFromL <> notesFromR
  passLeft <- case getInner sliceL of
    Nothing -> pure MS.empty
    Just (Notes notesl) ->
      samplePassing (MS.toList notesl) notes pNewPassingLeft
  passRight <- case getInner sliceR of
    Nothing -> pure MS.empty
    Just (Notes notesr) ->
      samplePassing notes (MS.toList notesr) pNewPassingRight
  -- combine all sampling results into split operation 
  let
    splitTs  = M.fromList $ Bi.second (fmap fst) <$> childrenT
    splitNTs = M.fromListWith (<>) $ Bi.second ((: []) . fst) <$> childrenNT
    fromLeft =
      M.fromList $ Bi.second (fmap (\(n, _, o) -> (n, o))) <$> childrenL
    fromRight =
      M.fromList $ Bi.second (fmap (\(n, _, o) -> (n, o))) <$> childrenR
    keepLeftT = catMaybes $ do -- List
      ((l, _), cs     ) <- childrenT
      ((m, _), (kl, _)) <- cs
      pure $ if kl then Just (l, Inner m) else Nothing
    keepLeftNT = mapMaybe
      (\((l, _), ((m, _), (kl, _))) ->
        if kl then Just (Inner l, Inner m) else Nothing
      )
      childrenNT
    keepLeftL = catMaybes $ do -- List
      (l, cs)      <- childrenL
      (m, keep, _) <- cs
      pure $ if keep then Just (Inner l, Inner m) else Nothing
    keepLeft   = S.fromList $ keepLeftT <> keepLeftNT <> keepLeftL
    keepRightT = catMaybes $ do -- List
      ((_, r), cs     ) <- childrenT
      ((m, _), (_, kr)) <- cs
      pure $ if kr then Just (Inner m, r) else Nothing
    keepRightNT = mapMaybe
      (\((_, r), ((m, _), (_, kr))) ->
        if kr then Just (Inner m, Inner r) else Nothing
      )
      childrenNT
    keepRightR = catMaybes $ do -- List
      (r, cs)      <- childrenR
      (m, keep, _) <- cs
      pure $ if keep then Just (Inner m, Inner r) else Nothing
    keepRight = S.fromList $ keepRightT <> keepRightNT <> keepRightR
  pure $ SplitOp { splitTs
                 , splitNTs
                 , fromLeft
                 , fromRight
                 , keepLeft
                 , keepRight
                 , passLeft
                 , passRight
                 }
 where
  sampleNeighbor :: Bool -> SPC -> m SPC
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

  sampleRootNote = do
    fifthsSign <- sampleConst Bernoulli 0.5
    fifthsN    <- sampleValue Geometric0 $ pInner . pRootFifths
    let interval = if fifthsSign then fifthsN else negate fifthsN
    pure $ spc interval

  sampleDoubleChild :: SPC -> SPC -> m (SPC, DoubleOrnament)
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

  sampleT :: Edge SPC -> m (Edge SPC, [((SPC, DoubleOrnament), (Bool, Bool))])
  sampleT (l, r) = do
    n <- sampleValue Geometric1 $ pInner . pElaborateRegular
    fmap (((l, r), ) . catMaybes) $ replicateM n $ case (l, r) of
      ((:⋊), (:⋉)) -> do
        child <- sampleRootNote
        pure $ Just ((child, RootNote), (False, False))
      (Inner pl, Inner pr) -> do
        (child, orn) <- sampleDoubleChild pl pr
        keepLeft     <- sampleValue Bernoulli $ pInner . pKeepL
        keepRight    <- sampleValue Bernoulli $ pInner . pKeepR
        pure $ Just ((child, orn), (keepLeft, keepRight))
      _ -> pure Nothing

  -- requires distance >= M2
  sampleChromPassing pl pr = do
    atLeft <- sampleValue Bernoulli $ pInner . pConnectChromaticLeftOverRight
    let dir   = if direction (pl `pto` pr) == GT then id else down
        child = if atLeft
          then pl +^ dir chromaticSemitone
          else pr -^ dir chromaticSemitone
    keepL <- sampleValue Bernoulli $ pInner . pKeepL
    keepR <- sampleValue Bernoulli $ pInner . pKeepR
    pure ((child, PassingMid), (keepL, keepR))

  sampleMidPassing pl pr = do
    child <- sampleNeighbor (direction (pl `pto` pr) == GT) pl
    keepL <- sampleValue Bernoulli $ pInner . pKeepL
    keepR <- sampleValue Bernoulli $ pInner . pKeepR
    pure ((child, PassingMid), (keepL, keepR))

  sampleNonMidPassing pl pr = do
    left <- sampleValue Bernoulli $ pInner . pPassLeftOverRight
    let dirUp = direction (pl `pto` pr) == GT
    if left
      then do
        child <- sampleNeighbor dirUp pl
        keepl <- sampleValue Bernoulli $ pInner . pKeepL
        pure ((child, PassingLeft), (keepl, True))
      else do
        child <- sampleNeighbor (not dirUp) pr
        keepr <- sampleValue Bernoulli $ pInner . pKeepR
        pure ((child, PassingRight), (True, keepr))

  sampleNT :: InnerEdge SPC -> m (InnerEdge SPC, ((SPC, Passing), (Bool, Bool)))
  sampleNT (pl, pr) = fmap ((pl, pr), ) $ case degree $ iabs $ pl `pto` pr of
    1 -> sampleChromPassing pl pr
    2 -> do
      connect <- sampleValue Bernoulli $ pInner . pConnect
      if connect then sampleMidPassing pl pr else sampleNonMidPassing pl pr
    _ -> sampleNonMidPassing pl pr

  sampleL :: SPC -> m (SPC, [(SPC, Bool, RightOrnament)])
  sampleL parent = do
    n <- sampleValue Geometric0 $ pInner . pElaborateL
    fmap (parent, ) $ replicateM n $ do
      rep  <- sampleValue Bernoulli $ pInner . pRepeatOverNeighbor
      keep <- sampleValue Bernoulli $ pInner . pKeepL
      if rep
        then pure (parent, keep, SingleRightRepeat)
        else do
          stepUp <- sampleConst Bernoulli 0.5
          child  <- sampleNeighbor stepUp parent
          pure (child, keep, SingleRightNeighbor)

  sampleR :: SPC -> m (SPC, [(SPC, Bool, LeftOrnament)])
  sampleR parent = do
    n <- sampleValue Geometric0 $ pInner . pElaborateR
    fmap (parent, ) $ replicateM n $ do
      rep  <- sampleValue Bernoulli $ pInner . pRepeatOverNeighbor
      keep <- sampleValue Bernoulli $ pInner . pKeepR
      if rep
        then pure (parent, keep, SingleLeftRepeat)
        else do
          stepUp <- sampleConst Bernoulli 0.5
          child  <- sampleNeighbor stepUp parent
          pure (child, keep, SingleLeftNeighbor)

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
        nother <- sampleValue (Binomial n) $ pInner . pNotesOnOtherSide
        pure $ ToLeft $ n - nother
      _ -> do
        nother <- sampleValue (Binomial n) $ pInner . pNotesOnOtherSide
        pure $ ToRight $ n - nother
    pure ((note, n), to)

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
