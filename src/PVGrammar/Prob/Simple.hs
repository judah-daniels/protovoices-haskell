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
import           Data.Tuple                     ( swap )
import qualified Internal.MultiSet             as MS
import           Musicology.Pitch              as MP
import           Lens.Micro.TH                  ( makeLenses )
import           GHC.Generics                   ( Generic )
import           Data.Traversable               ( for )

data PVParamsOuter f = PVParamsOuter
  { _pSingleFreeze :: f Beta Bernoulli
  , _pDoubleLeft :: f Beta Bernoulli
  , _pDoubleLeftFreeze :: f Beta Bernoulli
  , _pDoubleRightSplit :: f Beta Bernoulli
  }
  deriving (Generic)

deriving instance (Show (f Beta Bernoulli)) => Show (PVParamsOuter f)

makeLenses ''PVParamsOuter

data PVParamsInner f = PVParamsInner
  { _pElaborateRegular :: f Beta Geometric1
  , _pElaborateL :: f Beta Geometric0
  , _pElaborateR :: f Beta Geometric0
  , _pRootFifths :: f Beta Geometric0
  , _pKeepL :: f Beta Bernoulli
  , _pKeepR :: f Beta Bernoulli
  , _pRepeatOverNeighbor :: f Beta Bernoulli
  , _pNBChromatic :: f Beta Bernoulli
  , _pNBAlt :: f Beta Geometric0
  , _pRepeatLeftOverRight :: f Beta Bernoulli
  , _pConnect :: f Beta Bernoulli
  , _pConnectChromaticLeftOverRight :: f Beta Bernoulli
  , _pPassLeftOverRight :: f Beta Bernoulli
  , _pNewPassingLeft :: f Beta Geometric0
  , _pNewPassingRight :: f Beta Geometric0
  , _pNoteHoriDirection :: f (Dirichlet 3) (Categorical 3)
  , _pNotesOnOtherSide :: f Beta Binomial
  }
  deriving (Generic)

deriving instance ( Show (f Beta Bernoulli)
                  , Show (f Beta Geometric0)
                  , Show (f Beta Geometric1)
                  , Show (f (Dirichlet 3) (Categorical 3))
                  , Show (f Beta Binomial)
                  ) => Show (PVParamsInner f)

makeLenses ''PVParamsInner

data PVParams f = PVParams { _pOuter :: PVParamsOuter f
                           , _pInner :: PVParamsInner f }
  deriving (Generic)

deriving instance ( Show (f Beta Bernoulli)
                  , Show (f Beta Geometric0)
                  , Show (f Beta Geometric1)
                  , Show (f (Dirichlet 3) (Categorical 3))
                  , Show (f Beta Binomial)
                  ) => Show (PVParams f)

makeLenses ''PVParams

type PVProbs = PVParams ProbsRep
type PVProbsInner = PVParamsInner ProbsRep

type ContextSingle n = (StartStop (Notes n), Edges n, StartStop (Notes n))
type ContextDouble n
  = (StartStop (Notes n), Edges n, Notes n, Edges n, StartStop (Notes n))

sampleSingleStep
  :: _ => ContextSingle SPC -> m (Leftmost (Split SPC) Freeze SPC)
sampleSingleStep parents@(_, trans, _) = if freezable trans
  then do
    shouldFreeze <- sampleValue Bernoulli $ pOuter . pSingleFreeze
    if shouldFreeze
      then LMFreezeOnly <$> sampleFreeze parents
      else LMSplitOnly <$> sampleSplit parents
  else LMSplitOnly <$> sampleSplit parents

sampleDoubleStep
  :: _
  => ContextDouble SPC
  -> Bool
  -> m (Leftmost (Split SPC) Freeze (Hori SPC))
sampleDoubleStep parents@(sliceL, transL, sliceM, transR, sliceR) afterSplitRight
  = if afterSplitRight
    then do
      shouldSplitRight <- sampleValue Bernoulli $ pOuter . pDoubleRightSplit
      if shouldSplitRight
        then LMSplitRight <$> sampleSplit (Inner sliceM, transR, sliceR)
        else LMHorizontalize <$> sampleHori parents
    else do
      continueLeft <- sampleValue Bernoulli $ pOuter . pDoubleLeft
      if continueLeft
        then if freezable transL
          then do
            shouldFreeze <- sampleValue Bernoulli $ pOuter . pDoubleLeftFreeze
            if shouldFreeze
              then LMSplitLeft <$> sampleSplit (sliceL, transL, Inner sliceM)
              else LMFreezeLeft <$> sampleFreeze (sliceL, transL, Inner sliceM)
          else LMSplitLeft <$> sampleSplit (sliceL, transL, Inner sliceM)
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
  let notesT  = concatMap (\(_, ns) -> fst . fst <$> ns) childrenT
      notesNT = fmap (fst . fst . snd) childrenNT
      notesL  = concatMap (\(_, ns) -> fmap (\(n, _, _) -> n) ns) childrenL
      notesR  = concatMap (\(_, ns) -> fmap (\(n, _, _) -> n) ns) childrenR
      notes   = notesT <> notesNT <> notesL <> notesR
  passLeft  <- sampleNewPassing notes sliceL pNewPassingLeft
  passRight <- MS.map swap <$> sampleNewPassing notes sliceR pNewPassingRight
  let

    -- combine all sampling results into split operation 
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

sampleHori :: _ => ContextDouble n -> m (Hori n)
sampleHori (sliceL, transL, Notes sliceM, transR, sliceR) = do
  -- distribute notes
  dists <- mapM distNote $ MS.toOccurList sliceM
  let notesLeft = flip fmap dists $ \((note, n), to) -> case to of
        ToRight dl -> (note, n - dl)
        _          -> (note, n)
      notesRight = flip fmap dists $ \((note, n), to) -> case to of
        ToLeft dr -> (note, n - dr)
        _         -> (note, n)
  -- generate repetition edges
  -- generate passing edges
  -- construct result
  let distMap = HM.fromList (Bi.first fst <$> dists)
      edges   = undefined
  pure $ HoriOp distMap edges
 where
  -- distribute a note to the two child slices
  distNote (note, n) = do
    dir <- sampleValue Categorical $ pInner . pNoteHoriDirection
    to  <- case dir of
      0 -> pure ToBoth
      1 -> do
        nother <- sampleValue (Binomial n) $ pInner . pNotesOnOtherSide
        pure $ ToLeft $ n - nother
      _ -> do
        nother <- sampleValue (Binomial n) $ pInner . pNotesOnOtherSide
        pure $ ToRight $ n - nother
    pure ((note, n), to)


sampleNewPassing
  :: _
  => [SPC]
  -> StartStop (Notes SPC)
  -> Accessor PVParamsInner Beta Geometric0
  -> m (MS.MultiSet (InnerEdge SPC))
sampleNewPassing notes slice pNewPassing = case getInner slice of
  Nothing              -> pure MS.empty
  Just (Notes parents) -> fmap (MS.fromList . concat) $ sequence $ do -- List
    p <- MS.toList parents
    m <- notes
    guard $ iabs (p `pto` m) < major second'
    pure $ do -- m
      n <- sampleValue Geometric0 $ pInner . pNewPassing
      pure $ replicate n (p, m)
