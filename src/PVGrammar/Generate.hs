{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module PVGrammar.Generate where

import           PVGrammar
import           Common
import           Display

import           Musicology.Pitch

import qualified Control.Monad.Writer.Strict   as MW
import           Data.Monoid                    ( Endo(..) )
import qualified Data.Map.Strict               as M
import qualified Data.MultiSet                 as MS
import qualified Data.HashSet                  as S
import qualified Data.Set                      as OS
import           Control.Monad                  ( foldM )
import qualified Data.List                     as L
import           Data.Foldable                  ( toList )
import           Data.Hashable                  ( Hashable )

-- building operations
-- ===================

mkSplit :: MW.Writer (Split i) a -> Split i
mkSplit = MW.execWriter

splitT
  :: (Ord i, Hashable i)
  => StartStop (Pitch i)
  -> StartStop (Pitch i)
  -> Pitch i
  -> Ornament
  -> Bool
  -> Bool
  -> MW.Writer (Split i) ()
splitT l r c o kl kr = MW.tell
  $ SplitOp (M.singleton (l, r) [(c, o)]) M.empty M.empty M.empty kls krs
 where
  kls = if kl then S.singleton (l, Inner c) else S.empty
  krs = if kr then S.singleton (Inner c, r) else S.empty

splitNT
  :: (Ord i, Hashable i)
  => Pitch i
  -> Pitch i
  -> Pitch i
  -> Passing
  -> Bool
  -> Bool
  -> MW.Writer (Split i) ()
splitNT l r c o kl kr = MW.tell
  $ SplitOp M.empty (M.singleton (l, r) [(c, o)]) M.empty M.empty kls krs
 where
  kls =
    if o /= PassingRight && kl then S.singleton (Inner l, Inner c) else S.empty
  krs =
    if o /= PassingLeft && kr then S.singleton (Inner c, Inner r) else S.empty

addToLeft
  :: (Ord i, Hashable i)
  => Pitch i
  -> Pitch i
  -> RightOrnament
  -> Bool
  -> MW.Writer (Split i) ()
addToLeft parent child op keep = MW.tell $ SplitOp
  M.empty
  M.empty
  (M.singleton parent [(child, op)])
  M.empty
  (if keep then S.singleton (Inner parent, Inner child) else S.empty)
  S.empty

addToRight
  :: (Ord i, Hashable i)
  => Pitch i
  -> Pitch i
  -> LeftOrnament
  -> Bool
  -> MW.Writer (Split i) ()
addToRight parent child op keep = MW.tell $ SplitOp
  M.empty
  M.empty
  M.empty
  (M.singleton parent [(child, op)])
  S.empty
  (if keep then S.singleton (Inner child, Inner parent) else S.empty)


mkHori :: MW.Writer (Endo (Hori i)) () -> Hori i
mkHori actions = appEndo (MW.execWriter actions) emptyHori
  where emptyHori = HoriOp M.empty $ Edges S.empty MS.empty

horiNote
  :: (Ord i, Hashable i)
  => Pitch i
  -> HoriDirection
  -> Bool
  -> MW.Writer (Endo (Hori i)) ()
horiNote pitch dir edge = MW.tell $ Endo h
 where
  h (HoriOp dist (Edges mTs mNTs)) = HoriOp dist' (Edges mTs' mNTs)
   where
    dist' = M.insert pitch dir dist
    mTs'  = S.union mTs
      $ if edge then S.singleton (Inner pitch, Inner pitch) else S.empty

addPassing :: Ord i => Pitch i -> Pitch i -> MW.Writer (Endo (Hori i)) ()
addPassing l r = MW.tell $ Endo h
 where
  h (HoriOp dist (Edges mTs mNTs)) = HoriOp dist (Edges mTs mNTs')
    where mNTs' = MS.insert (l, r) mNTs

-- applying operations
-- ===================

applySplit
  :: forall i
   . (Ord i, Notation (Pitch i), Hashable i)
  => Split i
  -> Edges i
  -> Either String (Edges i, Notes i, Edges i)
applySplit inSplit@(SplitOp splitTs splitNTs ls rs kl kr) inTop@(Edges topTs topNTs)
  = do
    notesT                       <- applyTs topTs splitTs
    (notesNT, leftNTs, rightNTs) <- applyNTs topNTs splitNTs
    let notesL = collectNotes ls
        notesR = collectNotes rs
        notes  = MS.unions [notesT, notesNT, notesL, notesR]
    pure (Edges kl leftNTs, Notes notes, Edges kr rightNTs)
 where

  allOps opset = do
    (parent, children) <- M.toList opset
    child              <- children
    pure (parent, child)

  showEdge (p1, p2) = showNotation p1 <> "-" <> showNotation p2
  showEdges ts = "{" <> L.intercalate "," (showEdge <$> toList ts) <> "}"

  applyTs top ops = do
    (top', notes) <- foldM (applyT top) (top, MS.empty) $ allOps ops
    if S.null top'
      then Right notes
      else
        Left $ "did not use all terminal edges, remaining: " <> showEdges top'

  applyT topAll (top, notes) (parent@(pl, pr), (note, _))
    | parent `S.member` topAll
    = Right (top', notes')
    | otherwise
    = Left
      $  "used non-existing terminal edge\n  top="
      <> show inTop
      <> "\n  split="
      <> show inSplit
   where
    top'   = S.delete parent top
    notes' = MS.insert note notes

  applyNTs top ops = do
    (top', notes, lNTs, rNTs) <-
      foldM applyNT (top, MS.empty, MS.empty, MS.empty) $ allOps ops
    if MS.null top'
      then Right (notes, lNTs, rNTs)
      else
        Left
        $  "did not use all non-terminal edges, remaining: "
        <> showEdges top'

  applyNT (top, notes, lNTs, rNTs) (parent@(pl, pr), (note, pass))
    | parent `MS.member` top
    = Right (top', notes', lNTs', rNTs')
    | otherwise
    = Left
      $  "used non-existing non-terminal edge\n  top="
      <> show inTop
      <> "\n  split="
      <> show inSplit
   where
    top'         = MS.delete parent top
    notes'       = MS.insert note notes
    (newl, newr) = case pass of
      PassingMid   -> (MS.empty, MS.empty)
      PassingLeft  -> (MS.empty, MS.singleton (note, pr))
      PassingRight -> (MS.singleton (pl, note), MS.empty)
    lNTs' = MS.union newl lNTs
    rNTs' = MS.union newr rNTs

  singleChild (_, (note, _)) = note
  collectNotes ops = MS.fromList $ singleChild <$> allOps ops

applyFreeze :: Eq i => Freeze -> Edges i -> Either String (Edges i)
applyFreeze FreezeOp e@(Edges ts nts)
  | not $ MS.null nts  = Left "cannot freeze non-terminal edges"
  | not $ all isRep ts = Left "cannot freeze non-tie edges"
  | otherwise          = Right e
  where isRep (a, b) = a == b

applyHori
  :: forall i
   . (Ord i, Notation (Pitch i))
  => Hori i
  -> Edges i
  -> Notes i
  -> Edges i
  -> Either String (Edges i, Notes i, Edges i, Notes i, Edges i)
applyHori (HoriOp dist childm) pl (Notes notesm) pr = do
  (notesl, notesr) <- foldM applyDist (MS.empty, MS.empty)
    $ MS.toOccurList notesm
  childl <- fixEdges snd pl notesl
  childr <- fixEdges fst pr notesr
  pure (childl, Notes notesl, childm, Notes notesr, childr)
 where
  applyDist (notesl, notesr) (note, n) = do
    d <-
      maybe (Left $ showNotation note <> " is not distributed") Right
        $ M.lookup note dist
    case d of
      ToBoth -> pure (MS.insertMany note n notesl, MS.insertMany note n notesr)
      ToLeft i -> if i > n || i <= 0
        then Left "moving more notes than allowed to the right"
        else pure
          (MS.insertMany note n notesl, MS.insertMany note (n - i) notesr)
      ToRight i -> if i > n || i <= 0
        then Left "moving more notes than allowed to the left"
        else pure
          (MS.insertMany note (n - i) notesl, MS.insertMany note n notesr)
  fixEdges
    :: (forall a . (a, a) -> a)
    -> Edges i
    -> MS.MultiSet (Pitch i)
    -> Either String (Edges i)
  fixEdges accessor (Edges ts nts) notesms
    | not $ all ((`OS.member` notes) . accessor) nts = Left
      "dropping non-terminal edge in hori"
    | otherwise = pure $ Edges ts' nts
   where
    notes  = MS.toSet notesms
    notesi = OS.map Inner notes
    ts'    = S.filter ((`OS.member` notesi) . accessor) ts

-- derivation player
-- =================

derivationPlayerPV
  :: (Eq i, Ord i, Notation (Pitch i), Hashable i)
  => DerivationPlayer (Split i) Freeze (Hori i) (Notes i) (Edges i)
derivationPlayerPV = DerivationPlayer topEdges
                                      topNotes
                                      applySplit
                                      applyFreeze
                                      applyHori
 where
  topEdges (:⋊) (:⋉) = Edges (S.singleton ((:⋊), (:⋉))) MS.empty
  topEdges _    _    = Edges S.empty MS.empty
  topNotes = Notes MS.empty