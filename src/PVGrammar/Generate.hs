{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module PVGrammar.Generate
  ( mkSplit
  , splitT
  , splitNT
  , addToLeft
  , addToRight
  , mkSpread
  , spreadNote
  , addPassing
  , addOctaveRepetition
  , addPassingLeft
  , addPassingRight
  , derivationPlayerPV
  , derivationPlayerPVAllEdges
  , applySplit
  , applySplitAllEdges
  , applyFreeze
  , applySpread
  , freezable
  , debugPVAnalysis
  , checkDerivation
  ) where

import Common
import Display
import PVGrammar hiding
  ( splitNTs
  , splitTs
  )

import Musicology.Pitch (Notation (..))

import Control.Monad (foldM)
import qualified Control.Monad.Writer.Strict as MW
import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Monoid (Endo (..))
import qualified Internal.MultiSet as MS
import qualified Musicology.Core as MC
  ( HasPitch (pitch)
  , Pitched (IntervalOf)
  )

-- building operations
-- ===================

mkSplit :: MW.Writer (Split n) a -> Split n
mkSplit = MW.execWriter

splitT
  :: (Ord n, Hashable n)
  => StartStop n
  -> StartStop n
  -> n
  -> DoubleOrnament
  -> Bool
  -> Bool
  -> MW.Writer (Split n) ()
splitT l r c o kl kr =
  MW.tell $
    SplitOp
      (M.singleton (l, r) [(c, o)])
      M.empty
      M.empty
      M.empty
      kls
      krs
      MS.empty
      MS.empty
 where
  kls = if kl then S.singleton (l, Inner c) else S.empty
  krs = if kr then S.singleton (Inner c, r) else S.empty

splitNT
  :: (Ord n, Hashable n)
  => n
  -> n
  -> n
  -> Passing
  -> Bool
  -> Bool
  -> MW.Writer (Split n) ()
splitNT l r c o kl kr =
  MW.tell $
    SplitOp
      M.empty
      (M.singleton (l, r) [(c, o)])
      M.empty
      M.empty
      kls
      krs
      MS.empty
      MS.empty
 where
  kls =
    if o /= PassingRight && kl then S.singleton (Inner l, Inner c) else S.empty
  krs =
    if o /= PassingLeft && kr then S.singleton (Inner c, Inner r) else S.empty

addToLeft
  :: (Ord n, Hashable n)
  => n
  -> n
  -> RightOrnament
  -> Bool
  -> MW.Writer (Split n) ()
addToLeft parent child op keep =
  MW.tell $
    SplitOp
      M.empty
      M.empty
      (M.singleton parent [(child, op)])
      M.empty
      (if keep then S.singleton (Inner parent, Inner child) else S.empty)
      S.empty
      MS.empty
      MS.empty

addToRight
  :: (Ord n, Hashable n)
  => n
  -> n
  -> LeftOrnament
  -> Bool
  -> MW.Writer (Split n) ()
addToRight parent child op keep =
  MW.tell $
    SplitOp
      M.empty
      M.empty
      M.empty
      (M.singleton parent [(child, op)])
      S.empty
      (if keep then S.singleton (Inner child, Inner parent) else S.empty)
      MS.empty
      MS.empty

addPassingLeft :: (Ord n, Hashable n) => n -> n -> MW.Writer (Split n) ()
addPassingLeft l m = MW.tell $ mempty{passLeft = MS.singleton (l, m)}

addPassingRight :: (Ord n, Hashable n) => n -> n -> MW.Writer (Split n) ()
addPassingRight m r = MW.tell $ mempty{passRight = MS.singleton (m, r)}

mkSpread :: MW.Writer (Endo (Spread n)) () -> Spread n
mkSpread actions = appEndo (MW.execWriter actions) emptySpread
 where
  emptySpread = SpreadOp HM.empty $ Edges S.empty MS.empty

spreadNote
  :: (Ord n, Hashable n)
  => n
  -> SpreadDirection
  -> Bool
  -> MW.Writer (Endo (Spread n)) ()
spreadNote pitch dir edge = MW.tell $ Endo h
 where
  h (SpreadOp dist (Edges mTs mNTs)) = SpreadOp dist' (Edges mTs' mNTs)
   where
    dist' = HM.insert pitch dir dist
    mTs' =
      S.union mTs $
        if edge then S.singleton (Inner pitch, Inner pitch) else S.empty

addPassing :: (Ord n, Hashable n) => n -> n -> MW.Writer (Endo (Spread n)) ()
addPassing l r = MW.tell $ Endo h
 where
  h (SpreadOp dist (Edges mTs mNTs)) = SpreadOp dist (Edges mTs mNTs')
   where
    mNTs' = MS.insert (l, r) mNTs

addOctaveRepetition
  :: (Ord n, Hashable n) => n -> n -> MW.Writer (Endo (Spread n)) ()
addOctaveRepetition l r = MW.tell $ Endo h
 where
  h (SpreadOp dist (Edges mTs mNTs)) = SpreadOp dist (Edges mTs' mNTs)
   where
    mTs' = S.insert (Inner l, Inner r) mTs

-- applying operations
-- ===================

applySplit
  :: forall n
   . (Ord n, Notation n, Hashable n)
  => Split n
  -> Edges n
  -> Either String (Edges n, Notes n, Edges n)
applySplit inSplit@(SplitOp splitTs splitNTs ls rs keepl keepr passl passr) inTop@(Edges topTs topNTs) =
  do
    notesT <- applyTs topTs splitTs
    (notesNT, leftNTs, rightNTs) <- applyNTs topNTs splitNTs
    let notesL = collectNotes ls
        notesR = collectNotes rs
        notes = MS.unions [notesT, notesNT, notesL, notesR]
    pure
      ( Edges keepl (MS.union leftNTs passl)
      , Notes notes
      , Edges keepr (MS.union rightNTs passr)
      )
 where
  allOps opset = do
    (parent, children) <- M.toList opset
    child <- children
    pure (parent, child)

  showEdge (p1, p2) = showNotation p1 <> "-" <> showNotation p2
  showEdges ts = "{" <> L.intercalate "," (showEdge <$> toList ts) <> "}"

  applyTs top ops = do
    (top', notes) <- foldM (applyT top) (top, MS.empty) $ allOps ops
    if S.null top'
      then Right notes
      else Left $ "did not use all terminal edges, remaining: " <> showEdges top'

  applyT topAll (top, notes) (parent, (note, _))
    | parent `S.member` topAll =
        Right (top', notes')
    | otherwise =
        Left $
          "used non-existing terminal edge\n  top="
            <> show inTop
            <> "\n  split="
            <> show inSplit
   where
    top' = S.delete parent top
    notes' = MS.insert note notes

  applyNTs top ops = do
    (top', notes, lNTs, rNTs) <-
      foldM applyNT (top, MS.empty, MS.empty, MS.empty) $ allOps ops
    if MS.null top'
      then Right (notes, lNTs, rNTs)
      else
        Left $
          "did not use all non-terminal edges, remaining: "
            <> showEdges
              (MS.toList top')

  applyNT (top, notes, lNTs, rNTs) (parent@(pl, pr), (note, pass))
    | parent `MS.member` top =
        Right (top', notes', lNTs', rNTs')
    | otherwise =
        Left $
          "used non-existing non-terminal edge\n  top="
            <> show inTop
            <> "\n  split="
            <> show inSplit
   where
    top' = MS.delete parent top
    notes' = MS.insert note notes
    (newl, newr) = case pass of
      PassingMid -> (MS.empty, MS.empty)
      PassingLeft -> (MS.empty, MS.singleton (note, pr))
      PassingRight -> (MS.singleton (pl, note), MS.empty)
    lNTs' = MS.union newl lNTs
    rNTs' = MS.union newr rNTs

  singleChild (_, (note, _)) = note
  collectNotes ops = MS.fromList $ singleChild <$> allOps ops

freezable :: (Eq (MC.IntervalOf n), MC.HasPitch n) => Edges n -> Bool
freezable (Edges ts nts) = MS.null nts && all isRep ts
 where
  isRep (a, b) = fmap MC.pitch a == fmap MC.pitch b

applyFreeze
  :: (Eq (MC.IntervalOf n), MC.HasPitch n)
  => Freeze
  -> Edges n
  -> Either String (Edges n)
applyFreeze FreezeOp e@(Edges ts nts)
  | not $ MS.null nts = Left "cannot freeze non-terminal edges"
  | not $ all isRep ts = Left "cannot freeze non-tie edges"
  | otherwise = Right e
 where
  isRep (a, b) = fmap MC.pitch a == fmap MC.pitch b

applySpread
  :: forall n
   . (Ord n, Notation n, Hashable n)
  => Spread n
  -> Edges n
  -> Notes n
  -> Edges n
  -> Either String (Edges n, Notes n, Edges n, Notes n, Edges n)
applySpread (SpreadOp dist childm) pl (Notes notesm) pr = do
  (notesl, notesr) <-
    foldM applyDist (MS.empty, MS.empty) $
      MS.toOccurList notesm
  childl <- fixEdges snd pl notesl
  childr <- fixEdges fst pr notesr
  pure (childl, Notes notesl, childm, Notes notesr, childr)
 where
  applyDist (notesl, notesr) (note, n) = do
    d <-
      maybe (Left $ showNotation note <> " is not distributed") Right $
        HM.lookup note dist
    case d of
      ToBoth -> pure (MS.insertMany note n notesl, MS.insertMany note n notesr)
      ToLeft i ->
        if i > n || i <= 0
          then Left "moving more notes than allowed to the right"
          else
            pure
              (MS.insertMany note n notesl, MS.insertMany note (n - i) notesr)
      ToRight i ->
        if i > n || i <= 0
          then Left "moving more notes than allowed to the left"
          else
            pure
              (MS.insertMany note (n - i) notesl, MS.insertMany note n notesr)
  fixEdges
    :: (forall a. (a, a) -> a)
    -> Edges n
    -> MS.MultiSet n
    -> Either String (Edges n)
  fixEdges accessor (Edges ts nts) notesms
    | not $ MS.all ((`S.member` notes) . accessor) nts =
        Left
          "dropping non-terminal edge in spread"
    | otherwise = pure $ Edges ts' nts
   where
    notes = MS.toSet notesms
    notesi = S.map Inner notes
    ts' = S.filter ((`S.member` notesi) . accessor) ts

applySplitAllEdges
  :: forall n
   . (Ord n, Notation n, Hashable n)
  => Split n
  -> Edges n
  -> Either String (Edges n, Notes n, Edges n)
applySplitAllEdges inSplit@(SplitOp splitTs splitNTs ls rs _ _ passl passr) inTop@(Edges topTs topNTs) =
  do
    (notesT, leftTsReg, rightTsReg) <- applyTs topTs splitTs
    (notesNT, leftNTs, rightNTs, leftTsPass, rightTsPass) <-
      applyNTs
        topNTs
        splitNTs
    let notesL = collectNotes ls
        notesR = collectNotes rs
        notes = MS.unions [notesT, notesNT, notesL, notesR]
        leftSingleEdges = (\(p, (c, _)) -> (Inner p, Inner c)) <$> allOps ls
        rightSingleEdges = (\(p, (c, _)) -> (Inner c, Inner p)) <$> allOps rs
        edgesl = leftTsReg <> leftTsPass <> S.fromList leftSingleEdges
        edgesr = rightTsReg <> rightTsPass <> S.fromList rightSingleEdges
    pure
      ( Edges edgesl (MS.union leftNTs passl)
      , Notes notes
      , Edges edgesr (MS.union rightNTs passr)
      )
 where
  allOps opset = do
    (parent, children) <- M.toList opset
    child <- children
    pure (parent, child)

  showEdge (p1, p2) = showNotation p1 <> "-" <> showNotation p2
  showEdges ts = "{" <> L.intercalate "," (showEdge <$> toList ts) <> "}"

  applyTs top ops = do
    (notes, edgesl, edgesr) <-
      foldM (applyT top) (MS.empty, S.empty, S.empty) $
        allOps ops
    pure (notes, edgesl, edgesr)

  applyT topAll (notes, edgesl, edgesr) (parent, (note, _))
    | parent `S.member` topAll =
        Right (notes', edgesl', edgesr')
    | otherwise =
        Left $
          "used non-existing terminal edge\n  top="
            <> show inTop
            <> "\n  split="
            <> show inSplit
   where
    notes' = MS.insert note notes
    edgesl' = S.insert (fst parent, Inner note) edgesl
    edgesr' = S.insert (Inner note, snd parent) edgesr

  applyNTs top ops = do
    (top', notes, lNTs, rNTs, lTs, rTs) <-
      foldM applyNT (top, MS.empty, MS.empty, MS.empty, S.empty, S.empty) $
        allOps ops
    if MS.null top'
      then Right (notes, lNTs, rNTs, lTs, rTs)
      else
        Left $
          "did not use all non-terminal edges, remaining: "
            <> showEdges
              (MS.toList top')

  applyNT (top, notes, lNTs, rNTs, lTs, rTs) (parent@(pl, pr), (note, pass))
    | parent `MS.member` top =
        Right (top', notes', lNTs', rNTs', lTs', rTs')
    | otherwise =
        Left $
          "used non-existing non-terminal edge\n  top="
            <> show inTop
            <> "\n  split="
            <> show inSplit
   where
    top' = MS.delete parent top
    notes' = MS.insert note notes
    (newlNT, newrNT, newlT, newrT) = case pass of
      PassingMid ->
        ( MS.empty
        , MS.empty
        , S.singleton (Inner pl, Inner note)
        , S.singleton (Inner note, Inner pr)
        )
      PassingLeft ->
        ( MS.empty
        , MS.singleton (note, pr)
        , S.singleton (Inner pl, Inner note)
        , S.empty
        )
      PassingRight ->
        ( MS.singleton (pl, note)
        , MS.empty
        , S.empty
        , S.singleton (Inner note, Inner pr)
        )
    lNTs' = MS.union newlNT lNTs
    rNTs' = MS.union newrNT rNTs
    lTs' = S.union newlT lTs
    rTs' = S.union newrT rTs

  singleChild (_, (note, _)) = note
  collectNotes ops = MS.fromList $ singleChild <$> allOps ops

applyFreezeAllEdges FreezeOp e@(Edges ts nts)
  | not $ MS.null nts = Left "cannot freeze non-terminal edges"
  | otherwise = Right e

-- debugging analyses

debugPVAnalysis
  :: (Notation n, Ord n, Hashable n, MC.HasPitch n, Eq (MC.IntervalOf n))
  => PVAnalysis n
  -> IO (Either String ())
debugPVAnalysis = debugAnalysis applySplit applyFreeze applySpread

-- derivation player
-- =================

derivationPlayerPV
  :: (Eq n, Ord n, Notation n, Hashable n, Eq (MC.IntervalOf n), MC.HasPitch n)
  => DerivationPlayer (Split n) Freeze (Spread n) (Notes n) (Edges n)
derivationPlayerPV =
  DerivationPlayer
    topTrans
    applySplit
    applyFreeze
    applySpread
 where
  topTrans = Edges (S.singleton (Start, Stop)) MS.empty

derivationPlayerPVAllEdges
  :: (Eq n, Ord n, Notation n, Hashable n, Eq (MC.IntervalOf n), MC.HasPitch n)
  => DerivationPlayer (Split n) Freeze (Spread n) (Notes n) (Edges n)
derivationPlayerPVAllEdges =
  DerivationPlayer
    topTrans
    applySplitAllEdges
    applyFreezeAllEdges
    applySpread
 where
  topTrans = Edges (S.singleton (Start, Stop)) MS.empty

{- | Compares the output of a derivation
 with the original piece (as provided to the parser).
 Returns 'True' if the output matches the original
 and 'False' if the output doesn't match or the derivation is invalid.
-}
checkDerivation
  :: ( Ord n
     , Notation n
     , Hashable n
     , Eq (MC.IntervalOf n)
     , MC.HasPitch n
     , Show n
     )
  => [Leftmost (Split n) Freeze (Spread n)]
  -> Path [n] [Edge n]
  -> Bool
checkDerivation deriv original =
  case replayDerivation derivationPlayerPV deriv of
    (Left _) -> False
    (Right g) -> do
      let path' = case dgFrozen g of
            (_ : (_, tlast, slast) : rst) -> do
              s <- getInner $ dslContent slast
              foldM foldPath (PathEnd s, tlast) rst
            _ -> Nothing
          orig' =
            bimap
              (Notes . MS.fromList)
              (\e -> Edges (S.fromList e) MS.empty)
              original
      case path' of
        Nothing -> False
        Just (result, _) -> result == orig'
 where
  foldPath (pacc, tacc) (_, tnew, snew) = do
    s <- getInner $ dslContent snew
    pure (Path s tacc pacc, tnew)
