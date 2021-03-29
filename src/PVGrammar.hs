{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
module PVGrammar where

import           Common

import           Musicology.Core               as MT

import qualified Data.Set                      as S
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.MultiSet                 as MS
import           Data.Foldable                  ( toList
                                                , foldl'
                                                )
import           Control.Monad                  ( foldM
                                                , mzero
                                                )
import           Data.Maybe                     ( catMaybes
                                                , maybeToList
                                                )

-- types
-- =====

-- slice type: sets of notes
----------------------------

newtype Notes i = Notes (MS.MultiSet (Pitch i))
  deriving (Eq, Ord)

instance (Notation (Pitch i)) => Show (Notes i) where
  show (Notes ns) =
    "(" <> L.intercalate "," (showNotation <$> MS.toList ns) <> ")"

innerNotes :: StartStop (Notes i) -> [StartStop (Pitch i)]
innerNotes (Inner (Notes n)) = Inner <$> MS.elems n
innerNotes (:⋊)              = [(:⋊)]
innerNotes (:⋉)              = [(:⋉)]

-- transition type: sets of obligatory edges
--------------------------------------------

type Edge i = (StartStop (Pitch i), StartStop (Pitch i))

unpackEdge :: Edge i -> Maybe (Pitch i, Pitch i)
unpackEdge (Inner p1, Inner p2) = Just (p1, p2)
unpackEdge _                    = Nothing

data Edges i = Edges
               { edgesT  :: MS.MultiSet (Edge i)
               , edgesNT :: MS.MultiSet (Pitch i, Pitch i)
               }
  deriving (Eq, Ord)

instance (Notation (Pitch i)) => Show (Edges i) where
  show (Edges ts nts) =
    L.intercalate "," $ (showT <$> MS.toList ts) <> (showNT <$> MS.toList nts)
   where
    showT (n1, n2) = showNode n1 <> "-" <> showNode n2
    showNT (n1, n2) = showNotation n1 <> ">" <> showNotation n2
    showNode (:⋊)      = "⋊"
    showNode (:⋉)      = "⋉"
    showNode (Inner p) = showNotation p

-- operations
-- ==========

data Ornament = FullNeighbor
              | LeftNeighborOfRight
              | RightNeighborOfLeft
              | FullRepeat
              | LeftRepeatOfRight
              | RightRepeatOfLeft
              | Passing
              | RootNote

data Split i = SplitOp
  { splitTs :: M.Map (Edge i) [(Pitch i, Ornament, Bool, Bool)]
  , splitNTs :: M.Map (Pitch i, Pitch i) [(Pitch i, Bool, Bool)]
  }

data Freeze = FreezeOp

data HoriDirection = ToLeft Int  -- ^ all to the left, n fewer to the right
                   | ToRight Int -- ^ all to the right, n fewer to the left
                   | ToBoth      -- ^ all to both

data Hori i = HoriOp (M.Map (Pitch i) HoriDirection) (Edges i)

type PVLeftMost i = Leftmost (Split i) Freeze (Hori i)

-- parsing Ornamentations
-- ======================

-- TODO: recursive one-sided neighbors are not covered at the moment

between pl pm pr =
  pl /= pm && pm /= pr && pl /= pr && dir1 == odir && dir2 == odir
 where
  odir = direction $ pl `pto` pr
  dir1 = direction $ pl `pto` pm
  dir2 = direction $ pm `pto` pr

data EdgeEither a b = T a | NT b
  deriving (Eq, Ord, Show)

partitionEdgeEither :: Foldable t => t (EdgeEither a b) -> ([a], [b])
partitionEdgeEither = foldl' select ([], [])
 where
  select (ts, nts) (T  t ) = (t : ts, nts)
  select (ts, nts) (NT nt) = (ts, nt : nts)

findOrnament
  :: (Eq i, Diatonic i)
  => StartStop (Pitch i)
  -> StartStop (Pitch i)
  -> StartStop (Pitch i)
  -> Maybe (Ornament, EdgeEither (Edge i) (Pitch i, Pitch i))
findOrnament (Inner l) (Inner m) (Inner r)
  | l == m && m == r          = Just (FullRepeat, T (Inner l, Inner r))
  | l == m && so              = Just (RightRepeatOfLeft, T (Inner l, Inner r))
  | m == r && so              = Just (LeftRepeatOfRight, T (Inner l, Inner r))
  | l == r && s1              = Just (FullNeighbor, T (Inner l, Inner r))
  | s1 && so                  = Just (RightNeighborOfLeft, T (Inner l, Inner r))
  | s2 && so                  = Just (LeftNeighborOfRight, T (Inner l, Inner r))
  | s1 && s2 && between l m r = Just (Passing, NT (l, r))
 where
  s1 = isStep $ l `pto` m
  s2 = isStep $ m `pto` r
  so = isStep $ l `pto` r
findOrnament (:⋊) (Inner m) (Inner r)
  | m == r             = Just (LeftRepeatOfRight, T ((:⋊), Inner r))
  | isStep $ m `pto` r = Just (LeftNeighborOfRight, T ((:⋊), Inner r))
findOrnament (Inner l) (Inner m) (:⋉)
  | l == m             = Just (RightRepeatOfLeft, T (Inner l, (:⋉)))
  | isStep $ l `pto` m = Just (RightNeighborOfLeft, T (Inner l, (:⋉)))
findOrnament (:⋊) (Inner _) (:⋉) = Just (RootNote, T ((:⋊), (:⋉)))
findOrnament _    _         _    = Nothing

findPassing
  :: EdgeEither (StartStop (Pitch i)) (Pitch i)
  -> Pitch i
  -> EdgeEither (StartStop (Pitch i)) (Pitch i)
  -> Maybe (Pitch i, Pitch i)
findPassing (T  (Inner l)) m (NT r        ) = undefined
findPassing (NT l        ) m (T  (Inner r)) = undefined
findPassing _              _ _              = Nothing

-- edgeIsNT :: Diatonic i => Edge i -> Bool
-- edgeIsNT (Inner a, Inner b) = not $ isStep $ a `pto` b
-- edgeIsNT _                  = False

-- evaluator interface
-- ===================

protoVoiceEvaluator
  :: (Foldable t, Eq i, Ord i, Diatonic i)
  => Eval (Edges i) (t (Edge i)) (Notes i) (PVLeftMost i)
protoVoiceEvaluator = Eval pvVertMiddle pvVertLeft pvVertRight pvMerge pvThaw

pvVertMiddle :: (Eq i, Ord i) => VertMiddle (Edges i) (Notes i) (PVLeftMost i)
pvVertMiddle (Notes nl, edges, Notes nr) =
  if any notARepetition (edgesNT edges) then Nothing else Just (Notes top, op)
 where
  notARepetition (p1, p2) = p1 /= p2
  top     = MS.maxUnion nl nr
  leftMS  = nl MS.\\ nr
  left    = M.fromList $ fmap ToLeft <$> MS.toOccurList leftMS
  rightMS = nr MS.\\ nl
  right   = M.fromList $ fmap ToRight <$> MS.toOccurList rightMS
  bothSet =
    S.intersection (MS.toSet nl) (MS.toSet nr)
      S.\\ (MS.toSet leftMS `S.union` MS.toSet rightMS)
  both = M.fromSet (const ToBoth) bothSet
  op   = LMHorizontalize $ HoriOp (left <> right <> both) edges

pvVertLeft :: VertLeft (Edges i) (Notes i) (PVLeftMost i)
pvVertLeft (el, sl) top = [el]

pvVertRight :: VertRight (Edges i) (Notes i) (PVLeftMost i)
pvVertRight (sr, er) top = [er]

pvMerge :: (Ord i, Diatonic i) => Merge (Edges i) (Notes i) (PVLeftMost i)
pvMerge notesl (Edges leftTs leftNTs) (Notes notesm) (Edges rightTs rightNTs) notesr is2nd
  = map mkTop combinations
 where
  innerL  = T <$> innerNotes notesl
  innerR  = T <$> innerNotes notesr

  -- find all reduction options for every pitch
  options = map noteOptions $ MS.toOccurList notesm
  noteOptions (note, n) = goL mandatoryLeft mandatoryRight n []
   where
    mleftTs        = MS.map (T . fst) $ MS.filter ((== Inner note) . snd) leftTs
    mleftNTs       = MS.map (NT . fst) $ MS.filter ((== note) . snd) leftNTs
    mrightTs = MS.map (T . snd) $ MS.filter ((== Inner note) . fst) rightTs
    mrightNTs      = MS.map (NT . snd) $ MS.filter ((== note) . fst) rightNTs
    mandatoryLeft  = mleftTs <> mleftNTs
    mandatoryRight = mrightTs <> mrightNTs

    -- stage 1: consume all mandatory edges on the left
    goL ml mr 0 acc | MS.null ml && MS.null mr = [partitionEdgeEither acc]
                    | otherwise                = []
    goL ml mr n acc = case MS.minView ml of
      Just (l, ml') -> do
        (new, mr') <- pickLeft l mr
        goL ml' mr' (n - 1) (new : acc)
      Nothing -> goR mr n acc
    pickLeft l mr = mand <> opt
     where
      mand = do
        r   <- MS.elems mr
        red <- maybeToList $ tryReduction True True l r
        pure (red, MS.delete r mr)
      -- TODO: remove mr options here?
      opt = fmap (, mr) $ catMaybes $ tryReduction True False l <$> innerR

    -- stage 2: consume all remaining mandatory edges on the right
    goR mr 0 acc | MS.null mr = [partitionEdgeEither acc]
                 | otherwise  = []
    goR mr n acc = case MS.minView mr of
      Just (r, mr') -> do
        new <- pickRight r
        goR mr' (n - 1) (new : acc)
      Nothing -> goFree n acc
    pickRight r = catMaybes ((\l -> tryReduction False True l r) <$> innerL)

    -- stage 3: explain all remaining notes through a combination of unknown edges
    goFree 0 acc = [partitionEdgeEither acc]
    goFree n acc = do
      new <- pickFree
      goFree (n - 1) (new : acc)
    pickFree = do
      l <- innerL
      r <- innerR
      maybeToList $ tryReduction False False l r

    -- at all stages: try out potential reductions
    tryReduction lchild rchild (T notel) (T noter) = do
      (orn, parent) <- findOrnament notel (Inner note) noter
      pure $ case parent of
        (T  parent) -> T (parent, (note, orn, lchild, rchild))
        (NT parent) -> NT (parent, (note, lchild, rchild))
    tryReduction lchild rchild notel@(NT _) noter@(T _) = do
      parent <- findPassing notel note noter
      pure $ NT (parent, (note, lchild, rchild))
    tryReduction lchild rchild notel@(T _) noter@(NT _) = do
      parent <- findPassing notel note noter
      pure $ NT (parent, (note, lchild, rchild))
    tryReduction _ _ _ _ = Nothing

  -- compute all possible combinations of reduction options
  combinations = foldM pickOption ([], []) options
  pickOption :: ([a0], [a1]) -> [([a0], [a1])] -> [([a0], [a1])]
  pickOption (accT, accNT) opts = do
    (ts, nts) <- opts
    pure (ts <> accT, nts <> accNT)

  -- convert a combination into an operation
  op = if is2nd then LMSplitRight else LMSplitLeft
  mkTop (ts, nts) =
    ( Edges (MS.fromList (fst <$> ts)) (MS.fromList (fst <$> nts))
    , op $ SplitOp tmap ntmap
    )
   where
    tmap  = M.fromListWith (<>) $ fmap (: []) <$> ts
    ntmap = M.fromListWith (<>) $ fmap (: []) <$> nts

pvThaw
  :: (Foldable t, Ord i)
  => StartStop (Notes i)
  -> t (Edge i)
  -> StartStop (Notes i)
  -> [(Edges i, PVLeftMost i)]
pvThaw l e r = [(Edges (MS.fromList $ toList e) MS.empty, LMFreeze FreezeOp)]
