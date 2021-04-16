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
import qualified Data.Set                      as S
import           Control.Monad                  ( foldM )

-- building operations
-- ===================

mkSplit :: MW.Writer (Split i) a -> Split i
mkSplit = MW.execWriter

splitT
  :: Ord i
  => StartStop (Pitch i)
  -> StartStop (Pitch i)
  -> Pitch i
  -> Ornament
  -> Bool
  -> Bool
  -> MW.Writer (Split i) ()
splitT l r c o kl kr = MW.tell
  $ SplitOp (M.singleton (l, r) [(c, o, kl, kr)]) M.empty M.empty M.empty

splitNT
  :: Ord i
  => Pitch i
  -> Pitch i
  -> Pitch i
  -> Bool
  -> Bool
  -> MW.Writer (Split i) ()
splitNT l r c kl kr =
  MW.tell $ SplitOp M.empty (M.singleton (l, r) [(c, kl, kr)]) M.empty M.empty

mkHori :: MW.Writer (Endo (Hori i)) () -> Hori i
mkHori actions = appEndo (MW.execWriter actions) emptyHori
  where emptyHori = HoriOp M.empty $ Edges MS.empty MS.empty

horiNote
  :: Ord i => Pitch i -> HoriDirection -> Int -> MW.Writer (Endo (Hori i)) ()
horiNote pitch dir edges = MW.tell $ Endo h
 where
  h (HoriOp dist (Edges mTs mNTs)) = HoriOp dist' (Edges mTs' mNTs)
   where
    dist' = M.insert pitch dir dist
    mTs'  = MS.insertMany (Inner pitch, Inner pitch) edges mTs

addPassing :: Ord i => Pitch i -> Pitch i -> MW.Writer (Endo (Hori i)) ()
addPassing l r = MW.tell $ Endo h
 where
  h (HoriOp dist (Edges mTs mNTs)) = HoriOp dist (Edges mTs mNTs')
    where mNTs' = MS.insert (l, r) mNTs

-- applying operations
-- ===================

applySplit
  :: forall i
   . (Ord i, Notation (Pitch i))
  => Split i
  -> Edges i
  -> Either String (Edges i, Notes i, Edges i)
applySplit inSplit@(SplitOp splitTs splitNTs ls rs) inTop@(Edges topTs topNTs)
  = do
    (leftNTs, rightNTs, notesNT) <- applySplits applyNT topNTs (allOps splitNTs)
    (leftTsSplit, rightTsSplit, notesT) <- applySplits
      applyT
      topTs
      (downcast <$> allOps splitTs)

    let (leftTsSingle , notesL) = applyFromLefts $ allOps ls
        (rightTsSingle, notesR) = applyFromRights $ allOps rs
        notes                   = MS.unions [notesT, notesNT, notesL, notesR]
        leftTs                  = MS.union leftTsSplit leftTsSingle
        rightTs                 = MS.union rightTsSplit rightTsSingle
    pure (Edges leftTs leftNTs, Notes notes, Edges rightTs rightNTs)
 where

  allOps opset = do
    (parent, children) <- M.toList opset
    child              <- children
    pure (parent, child)

  downcast (p, (n, _, l, r)) = (p, (n, l, r))

  applyNT = applySplit id
  applyT  = applySplit Inner
  applySplits f top ops = do
    (top', left, right, notes) <- foldM (f top)
                                        (top, MS.empty, MS.empty, MS.empty)
                                        ops
    if MS.null top'
      then Right (left, right, notes)
      else Left "did not use all edges"

  applySplit f topAll (top, left, right, notes) (parent@(pl, pr), (note, usedLeft, usedRight))
    | parent `MS.member` topAll
    = Right (top', left', right', notes')
    | otherwise
    = Left
      $  "used non-existing edge\n  top="
      <> show inTop
      <> "\n  split="
      <> show inSplit
   where
    top'   = MS.delete parent top
    notes' = MS.insert note notes
    left'  = if usedLeft then MS.insert (pl, f note) left else left
    right' = if usedRight then MS.insert (f note, pr) right else right

  singleChild (_, (note, _, _)) = note
  singleEdge mk (parent, (child, _, _)) = mk (Inner parent) (Inner child)
  singleKeep (_, (_, _, keep)) = keep

  applyFromLefts ls = (edges, notes)
   where
    edges = MS.fromList $ singleEdge (,) <$> filter singleKeep ls
    notes = MS.fromList $ singleChild <$> ls

  applyFromRights ls = (edges, notes)
   where
    edges = MS.fromList $ singleEdge (flip (,)) <$> filter singleKeep ls
    notes = MS.fromList $ singleChild <$> ls

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
    | not $ all ((`S.member` notes) . accessor) nts = Left
      "dropping non-terminal edge in hori"
    | otherwise = pure $ Edges ts' nts
   where
    notes  = MS.toSet notesms
    notesi = S.map Inner notes
    ts'    = MS.filter ((`S.member` notesi) . accessor) ts

-- derivation player
-- =================

derivationPlayerPV
  :: (Eq i, Ord i, Notation (Pitch i))
  => DerivationPlayer (Split i) Freeze (Hori i) (Notes i) (Edges i)
derivationPlayerPV = DerivationPlayer root applySplit applyFreeze applyHori
  where root = Edges (MS.singleton ((:⋊), (:⋉))) MS.empty
