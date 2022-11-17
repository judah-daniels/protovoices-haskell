{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- | This module contains functions for the generative aspects of protovoice derivations:

 - manually constructing protovoice operations (see "PVGrammar") using a monadic interface
 - applying ("replaying") these operations.
-}
module PVGrammar.Generate
  ( -- * Manually Constructing Derivations

    -- | The functions in this section can be used
    -- to manually construct individual derivation operations
    -- or in conjunction with the (indexed-)monadic functions in "Common" (see 'Common.buildDerivation')
    -- to manually construct complete derivations.
    -- Each outer-structure operation ('mkSplit', 'mkSpread', 'mkFreeze') enters a writer monad
    -- in which inner-structure operations can be chained to determine the details.
    --
    -- Note that the legality of the operations is not always checked, so be careful!

    -- * Freeze
    mkFreeze

    -- ** Split
  , mkSplit
  , splitRegular
  , splitPassing
  , addToLeft
  , addToRight
  , addPassingLeft
  , addPassingRight

    -- ** Spread
  , mkSpread
  , spreadNote
  , addPassing
  , addOctaveRepetition

    -- * Derivation Players

    -- | These players can be used with the replay functions in the "Display" module
    -- to obtain derivation graphs for protovoice derivations.
  , derivationPlayerPV
  , derivationPlayerPVAllEdges

    -- * Applying Operations

    -- | Apply operations to parent objects and get the resulting child objects.
  , applySplit
  , applySplitAllEdges
  , applyFreeze
  , applySpread
  , freezable

    -- * Utility Functions
  , debugPVAnalysis
  , checkDerivation
  ) where

import Common
import Display
import PVGrammar

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

{- | Create a freeze operation (alias for 'FreezeOp').
 Can be used together with the 'Common.freeze' action within a monadic derivation.
-}
mkFreeze :: Freeze
mkFreeze = FreezeOp

{- | Create a split operation monadically

 > mkSplit $ do
 >   ... -- internal split actions

 Can be used together with the 'Common.split' action within a monadic derivation.
-}
mkSplit :: MW.Writer (Split n) a -> Split n
mkSplit = MW.execWriter

-- | During a split, split an existing regular edge between two notes.
splitRegular
  :: (Ord n, Hashable n)
  => StartStop n
  -- ^ left parent
  -> StartStop n
  -- ^ right parent
  -> n
  -- ^ the new child note
  -> DoubleOrnament
  -- ^ the ornament type of the child note
  -> Bool
  -- ^ keep the left child edge (left parent to child)?
  -> Bool
  -- ^ keep the right child edge (child to right parent)?
  -> MW.Writer (Split n) ()
splitRegular l r c o kl kr =
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

-- | During a split, split an existing passing edge, introducing a new passing note.
splitPassing
  :: (Ord n, Hashable n)
  => n
  -- ^ left parent
  -> n
  -- ^ right parent
  -> n
  -- ^ the new child note
  -> PassingOrnament
  -- ^ the ornament type of the child note
  -> Bool
  -- ^ keep the left child edge (if step)
  -> Bool
  -- ^ keep the right child edge (if step)
  -> MW.Writer (Split n) ()
splitPassing l r c o kl kr =
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

-- | During a split, add a new single-sided ornament to a left parent note.
addToLeft
  :: (Ord n, Hashable n)
  => n
  -- ^ parent (from the left slice)
  -> n
  -- ^ the new child note
  -> RightOrnament
  -- ^ the new child note's ornament type
  -> Bool
  -- ^ keep the new edge?
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

-- | During a split, add a new single-sided ornament to a right parent note.
addToRight
  :: (Ord n, Hashable n)
  => n
  -- ^ parent (from the right slice)
  -> n
  -- ^ the new child note
  -> LeftOrnament
  -- ^ the new child note's ornament type
  -> Bool
  -- ^ keep the new edge?
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

-- | During a split, add a new passing edge between the left parent slice and the child slice.
addPassingLeft
  :: (Ord n, Hashable n)
  => n
  -- ^ note from the left parent slice
  -> n
  -- ^ note from the child slice
  -> MW.Writer (Split n) ()
addPassingLeft l m = MW.tell $ mempty{passLeft = MS.singleton (l, m)}

-- | During a split, add a new passing edge between the child slice and the right parent slice.
addPassingRight
  :: (Ord n, Hashable n)
  => n
  -- ^ note from the child slice
  -> n
  -- ^ note from the right parent slice
  -> MW.Writer (Split n) ()
addPassingRight m r = MW.tell $ mempty{passRight = MS.singleton (m, r)}

{- | Create a spread operation monadically

 > mkSpread $ do
 >   ... -- internal spread actions

 Can be used together with the 'Common.spread' action within a monadic derivation.
-}
mkSpread :: MW.Writer (Endo (Spread n)) () -> Spread n
mkSpread actions = appEndo (MW.execWriter actions) emptySpread
 where
  emptySpread = SpreadOp HM.empty $ Edges S.empty MS.empty

-- | During a spread, distribute one of the parent notes to the child slices of a spread.
spreadNote
  :: (Ord n, Hashable n)
  => n
  -- ^ the parent note
  -> SpreadDirection
  -- ^ the distribution of the note
  -> Bool
  -- ^ introduce a repetition edge (if possible)?
  -> MW.Writer (Endo (Spread n)) ()
spreadNote pitch dir edge = MW.tell $ Endo h
 where
  h (SpreadOp dist (Edges mRegs mPassings)) = SpreadOp dist' (Edges mRegs' mPassings)
   where
    dist' = HM.insert pitch dir dist
    mRegs' =
      S.union mRegs $
        if edge then S.singleton (Inner pitch, Inner pitch) else S.empty

-- | During a spread, add a new passing edge between the child slices of a spread.
addPassing
  :: (Ord n, Hashable n)
  => n
  -- ^ the left end of the edge
  -> n
  -- ^ the right end of the edge
  -> MW.Writer (Endo (Spread n)) ()
addPassing l r = MW.tell $ Endo h
 where
  h (SpreadOp dist (Edges mRegs mPassings)) = SpreadOp dist (Edges mRegs mPassings')
   where
    mPassings' = MS.insert (l, r) mPassings

{- | During a spread, add a new repetition edge
 between two notes of the same pitch class but from different octaves.
-}
addOctaveRepetition
  :: (Ord n, Hashable n)
  => n
  -- ^ the left end of the edge
  -> n
  -- ^ the right end of the edge
  -> MW.Writer (Endo (Spread n)) ()
addOctaveRepetition l r = MW.tell $ Endo h
 where
  h (SpreadOp dist (Edges mRegs mPassings)) = SpreadOp dist (Edges mRegs' mPassings)
   where
    mRegs' = S.insert (Inner l, Inner r) mRegs

-- applying operations
-- ===================

-- | Tries to apply a split operation to the parent transition.
applySplit
  :: forall n
   . (Ord n, Notation n, Hashable n)
  => Split n
  -- ^ the split operation
  -> Edges n
  -- ^ the parent transition
  -> Either String (Edges n, Notes n, Edges n)
  -- ^ the resulting child transitions and slice (or an error message).
applySplit inSplit@(SplitOp splitRegs splitPassings ls rs keepl keepr passl passr) inTop@(Edges topRegs topPassings) =
  do
    notesReg <- applyRegs topRegs splitRegs
    (notesPassing, leftPassings, rightPassings) <- applyPassings topPassings splitPassings
    let notesL = collectNotes ls
        notesR = collectNotes rs
        notes = MS.unions [notesReg, notesPassing, notesL, notesR]
    pure
      ( Edges keepl (MS.union leftPassings passl)
      , Notes notes
      , Edges keepr (MS.union rightPassings passr)
      )
 where
  allOps opset = do
    (parent, children) <- M.toList opset
    child <- children
    pure (parent, child)

  showEdge (p1, p2) = showNotation p1 <> "-" <> showNotation p2
  showEdges ts = "{" <> L.intercalate "," (showEdge <$> toList ts) <> "}"

  applyRegs top ops = do
    (top', notes) <- foldM (applyReg top) (top, MS.empty) $ allOps ops
    if S.null top'
      then Right notes
      else Left $ "did not use all terminal edges, remaining: " <> showEdges top'

  applyReg topAll (top, notes) (parent, (note, _))
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

  applyPassings top ops = do
    (top', notes, lPassings, rPassings) <-
      foldM applyPassing (top, MS.empty, MS.empty, MS.empty) $ allOps ops
    if MS.null top'
      then Right (notes, lPassings, rPassings)
      else
        Left $
          "did not use all non-terminal edges, remaining: "
            <> showEdges
              (MS.toList top')

  applyPassing (top, notes, lPassings, rPassings) (parent@(pl, pr), (note, pass))
    | parent `MS.member` top =
        Right (top', notes', lPassings', rPassings')
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
    lPassings' = MS.union newl lPassings
    rPassings' = MS.union newr rPassings

  singleChild (_, (note, _)) = note
  collectNotes ops = MS.fromList $ singleChild <$> allOps ops

-- | Indicates whether a transition can be frozen (i.e., doesn't contain non-"tie" edges).
freezable :: (Eq (MC.IntervalOf n), MC.HasPitch n) => Edges n -> Bool
freezable (Edges ts nts) = MS.null nts && all isRep ts
 where
  isRep (a, b) = fmap MC.pitch a == fmap MC.pitch b

-- | Tries to apply a freeze operation to a transition.
applyFreeze
  :: (Eq (MC.IntervalOf n), MC.HasPitch n)
  => Freeze
  -- ^ the freeze operation
  -> Edges n
  -- ^ the unfrozen edge
  -> Either String (Edges n)
  -- ^ the frozen transition
applyFreeze FreezeOp e@(Edges ts nts)
  | not $ MS.null nts = Left "cannot freeze non-terminal edges"
  | not $ all isRep ts = Left "cannot freeze non-tie edges"
  | otherwise = Right e
 where
  isRep (a, b) = fmap MC.pitch a == fmap MC.pitch b

-- | Tries to apply a spread operation to the parent transitions and slice.
applySpread
  :: forall n
   . (Ord n, Notation n, Hashable n)
  => Spread n
  -- ^ the spread operation
  -> Edges n
  -- ^ the left parent transition
  -> Notes n
  -- ^ the parent slice
  -> Edges n
  -- ^ the right parent transition
  -> Either String (Edges n, Notes n, Edges n, Notes n, Edges n)
  -- ^ the child transitions and slices (or an error message)
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

{- | A variant of 'applySplit' that inserts all protovoice edges into the child transitions,
 even those that are not "kept" (used for further elaboration).
 This is useful when you want to see all relations between notes in the piece.
-}
applySplitAllEdges
  :: forall n
   . (Ord n, Notation n, Hashable n)
  => Split n
  -> Edges n
  -> Either String (Edges n, Notes n, Edges n)
applySplitAllEdges inSplit@(SplitOp splitRegs splitPassings ls rs _ _ passl passr) inTop@(Edges topRegs topPassings) =
  do
    (notesReg, leftRegsReg, rightRegsReg) <- applyRegs topRegs splitRegs
    (notesPassing, leftPassings, rightPassings, leftRegsPass, rightRegsPass) <-
      applyPassings
        topPassings
        splitPassings
    let notesL = collectNotes ls
        notesR = collectNotes rs
        notes = MS.unions [notesReg, notesPassing, notesL, notesR]
        leftSingleEdges = (\(p, (c, _)) -> (Inner p, Inner c)) <$> allOps ls
        rightSingleEdges = (\(p, (c, _)) -> (Inner c, Inner p)) <$> allOps rs
        edgesl = leftRegsReg <> leftRegsPass <> S.fromList leftSingleEdges
        edgesr = rightRegsReg <> rightRegsPass <> S.fromList rightSingleEdges
    pure
      ( Edges edgesl (MS.union leftPassings passl)
      , Notes notes
      , Edges edgesr (MS.union rightPassings passr)
      )
 where
  allOps opset = do
    (parent, children) <- M.toList opset
    child <- children
    pure (parent, child)

  showEdge (p1, p2) = showNotation p1 <> "-" <> showNotation p2
  showEdges ts = "{" <> L.intercalate "," (showEdge <$> toList ts) <> "}"

  applyRegs top ops = do
    (notes, edgesl, edgesr) <-
      foldM (applyReg top) (MS.empty, S.empty, S.empty) $
        allOps ops
    pure (notes, edgesl, edgesr)

  applyReg topAll (notes, edgesl, edgesr) (parent, (note, _))
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

  applyPassings top ops = do
    (top', notes, lPassings, rPassings, lRegs, rRegs) <-
      foldM applyPassing (top, MS.empty, MS.empty, MS.empty, S.empty, S.empty) $
        allOps ops
    if MS.null top'
      then Right (notes, lPassings, rPassings, lRegs, rRegs)
      else
        Left $
          "did not use all non-terminal edges, remaining: "
            <> showEdges
              (MS.toList top')

  applyPassing (top, notes, lPassings, rPassings, lRegs, rRegs) (parent@(pl, pr), (note, pass))
    | parent `MS.member` top =
        Right (top', notes', lPassings', rPassings', lRegs', rRegs')
    | otherwise =
        Left $
          "used non-existing non-terminal edge\n  top="
            <> show inTop
            <> "\n  split="
            <> show inSplit
   where
    top' = MS.delete parent top
    notes' = MS.insert note notes
    (newlPassing, newrPassing, newlReg, newrReg) = case pass of
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
    lPassings' = MS.union newlPassing lPassings
    rPassings' = MS.union newrPassing rPassings
    lRegs' = S.union newlReg lRegs
    rRegs' = S.union newrReg rRegs

  singleChild (_, (note, _)) = note
  collectNotes ops = MS.fromList $ singleChild <$> allOps ops

{- | A variant of 'applyFreeze' that allows non-"tie" edges in the open transition.
 This is useful in conjunction with 'applySplitAllEdges'
 because the non-tie edges will not be dropped before freezing.
-}
applyFreezeAllEdges FreezeOp e@(Edges ts nts)
  | not $ MS.null nts = Left "cannot freeze non-terminal edges"
  | otherwise = Right e

-- debugging analyses

{- | A specialized version of 'debugAnalysis' for protovoice derivations.
 Prints the steps and intermediate configurations of a derivation.
-}
debugPVAnalysis
  :: (Notation n, Ord n, Hashable n, MC.HasPitch n, Eq (MC.IntervalOf n))
  => PVAnalysis n
  -> IO (Either String ())
debugPVAnalysis = debugAnalysis applySplit applyFreeze applySpread

-- derivation player
-- =================

{- | A derivation player for protovoices.
 The default version of the PV player drops all edges that are not used later on
 when generating child transitions.
 This behaviour matches the intermediate representation of the parsers,
 which only track edges that are necessary to explain the downstream notes.
 If you want to generate all edges (i.e., all functional relations between notes)
 use 'derivationPlayerPVAllEdges'.
-}
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

{- | A derivation player for protovoices that produces all edges
 that express a functional relation between two notes.
 For a version that only produces "necessary" edges, use 'derivationPlayerPV'.
-}
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
