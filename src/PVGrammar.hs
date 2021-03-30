{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
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
import           Data.Either                    ( partitionEithers )
import           Data.Maybe                     ( catMaybes
                                                , maybeToList
                                                )

-- types
-- =====

-- slice type: sets of notes
----------------------------

-- | The content type of 'Slice's.
-- Contains a multiset of pitches, representing the notes in a slice.
newtype Notes i = Notes (MS.MultiSet (Pitch i))
  deriving (Eq, Ord)

instance (Notation (Pitch i)) => Show (Notes i) where
  show (Notes ns) =
    "(" <> L.intercalate "," (showNotation <$> MS.toList ns) <> ")"

-- | Return the notes or start/stop symbols inside a slice.
-- This is useful to get all objects that an 'Edge' can connect to. 
innerNotes :: StartStop (Notes i) -> [StartStop (Pitch i)]
innerNotes (Inner (Notes n)) = Inner <$> MS.distinctElems n
innerNotes (:⋊)              = [(:⋊)]
innerNotes (:⋉)              = [(:⋉)]

-- transition type: sets of obligatory edges
--------------------------------------------

-- TODO: could this be improved to forbid start/stop symbols on the wrong side?
-- | A proto-voice edge between two nodes (i.e. notes or start/stop symbols).
type Edge i = (StartStop (Pitch i), StartStop (Pitch i))

-- | A proto-voice edge between two notes (excluding start/stop symbols).
type InnerEdge i = (Pitch i, Pitch i)

-- | The content type of 'Transition's.
-- Contains a multiset of normal (terminal) edges and a multiset of (non-terminal) passing edges.
-- The represented edges are those that are definitely used later on.
-- Edges that are not used are dropped before creating a child transition.
-- A transition that contains passing edges cannot be frozen.
data Edges i = Edges
               { edgesT  :: MS.MultiSet (Edge i)      -- ^ the terminal edges
               , edgesNT :: MS.MultiSet (InnerEdge i) -- ^ the non-terminal edges
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

-- helper type: Either for terminal and non-terminal edges
-- -------------------------------------------------------

-- | A tag that distinguishes between objects related to terminal and non-terminal edges.
-- Like 'Either', but with semantic constructor names to avoid confusion.
data EdgeEither a b = T a  -- ^ marks an terminal edge (or some related object)
                    | NT b -- ^ marks a non-terminal edge (or some related object)
  deriving (Eq, Ord, Show)

-- | Separates 'T's and 'NT's in a collection into a pair of lists for each type.
-- Analogous to 'partitionEithers'.
partitionEdgeEithers :: Foldable t => t (EdgeEither a b) -> ([a], [b])
partitionEdgeEithers = foldl' select ([], [])
 where
  select (ts, nts) (T  t ) = (t : ts, nts)
  select (ts, nts) (NT nt) = (ts, nt : nts)

-- operations
-- ==========

-- | Marks different types of ornaments in the derivation.
data Ornament = FullNeighbor
              | LeftNeighborOfRight
              | RightNeighborOfLeft
              | FullRepeat
              | LeftRepeatOfRight
              | RightRepeatOfLeft
              | Passing
              | RootNote
  deriving (Eq, Ord, Show)

-- | Encodes the decisions made in a split operation.
-- Contains a list of elaborations for every parent edge.
-- Each elaboration contains the child pitch,
-- the corresponding ornament, and flags for whether to keep either of the child edges.
data Split i = SplitOp
  { splitTs :: M.Map (Edge i) [(Pitch i, Ornament, Bool, Bool)]
  , splitNTs :: M.Map (InnerEdge i) [(Pitch i, Bool, Bool)]
  }
  deriving (Eq, Ord)

deriving instance (Show (Pitch i)) => Show (Split i)

-- | Represents a freeze operation.
-- Since this just ties all remaining edges
-- (which must all be repetitions)
-- no decisions have to be encoded.
data Freeze = FreezeOp
  deriving (Eq, Ord, Show)

-- | Encodes the distribution of a pitch in a horizontalization.
-- 
-- All instances of a pitch must be either moved completely to the left or the right (or both).
-- In addition, some instances may be repeated on the other side.
-- The difference is indicated by the field of the 'ToLeft' and 'ToRight' constructors.
-- For example, @ToLeft 3@ indicates that out of @n@ instances,
-- all @n@ are moved to the left and @n-3@ are replicated on the right.
data HoriDirection = ToLeft Int  -- ^ all to the left, n fewer to the right
                   | ToRight Int -- ^ all to the right, n fewer to the left
                   | ToBoth      -- ^ all to both
  deriving (Eq, Ord, Show)

-- | Represents a horzontalization operation.
-- Records for every pitch how it is distributed (see 'HoriDirection').
-- The resulting edges (repetitions and passing edges) are represented in a child transition.
data Hori i = HoriOp (M.Map (Pitch i) HoriDirection) (Edges i)
  deriving (Eq, Ord)

deriving instance (Notation (Pitch i), Show (Pitch i)) => Show (Hori i)

-- | 'Leftmost' specialized to the split, freeze, and horizontalize operations of the grammar.
type PVLeftMost i = Leftmost (Split i) Freeze (Hori i)

-- parsing Ornamentations
-- ======================

-- | Checks if `pm` is between `pl` and `pr`.
between pl pm pr =
  pl /= pm && pm /= pr && pl /= pr && dir1 == odir && dir2 == odir
 where
  odir = direction $ pl `pto` pr
  dir1 = direction $ pl `pto` pm
  dir2 = direction $ pm `pto` pr

-- TODO: recursive one-sided neighbors are not covered at the moment

-- | Attempts to reduce three nodes using an ornamentation operation.
-- If succesfull, returns the ornament type and the parent edge,
-- which is either a non-terminal edge for passing notes,
-- or a terminal edge for all other operations.
findOrnament
  :: (Eq i, Diatonic i)
  => StartStop (Pitch i)
  -> StartStop (Pitch i)
  -> StartStop (Pitch i)
  -> Maybe (Ornament, EdgeEither (Edge i) (InnerEdge i))
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

-- | Attempts to reduce three notes as a passing motion
-- where one of the child edges is a non-terminal edge.
--
-- Since one of the edges is a terminal edge,
-- the corresponding outer note could be start/stop symbol, in which case the reduction fails.
-- The side with the terminal edge is thus a @StartStop Pitch i@ within a 'T',
-- while the non-terminal side is a @Pitch i@ within an 'NT'.
-- Exactly one side must be a 'T' and the other an 'NT', otherwise the reduction fails.
findPassing
  :: (Diatonic i, Eq i)
  => EdgeEither (StartStop (Pitch i)) (Pitch i)
  -> Pitch i
  -> EdgeEither (StartStop (Pitch i)) (Pitch i)
  -> Maybe (InnerEdge i)
findPassing (T (Inner l)) m (NT r) | isStep (l `pto` m) && between l m r =
  Just (l, r)
findPassing (NT l) m (T (Inner r)) | isStep (m `pto` r) && between l m r =
  Just (l, r)
findPassing _ _ _ = Nothing

-- evaluator interface
-- ===================

-- | The evaluator that represents the proto-voice grammar.
-- As scores it returns a representation of each operation.
-- These scores do not form a semiring,
-- but can be embedded into different semirings using 'evalMapScores'.
protoVoiceEvaluator
  :: ( Foldable t
     , Eq i
     , Ord i
     , Diatonic i
     , Foldable t2
     , i ~ ICOf i'
     , Interval i'
     )
  => Eval (Edges i) (t (Edge i)) (Notes i) (t2 (Pitch i')) (PVLeftMost i)
protoVoiceEvaluator =
  Eval pvVertMiddle pvVertLeft pvVertRight pvMerge pvThaw pvSlice

-- | Computes the verticalization of a middle transition.
-- If the verticalization is admitted, returns the corresponding operation.
pvVertMiddle :: (Eq i, Ord i) => VertMiddle (Edges i) (Notes i) (PVLeftMost i)
pvVertMiddle (Notes nl, edges, Notes nr)
  | any notARepetition (edgesNT edges) = Nothing
  | otherwise                          = Just (Notes top, op)
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

-- | Computes all left parent transitions for a verticalization and a left child transition.
-- Here, this operation is always admitted and unique,
-- so the edges from the child transition are just passed through.
pvVertLeft :: VertLeft (Edges i) (Notes i) (PVLeftMost i)
pvVertLeft (el, sl) top = [el]

-- | Computes all right parent transition for a verticalization and a right child transition.
-- Here, this operation is always admitted and unique,
-- so the edges from the child transition are just passed through.
pvVertRight :: VertRight (Edges i) (Notes i) (PVLeftMost i)
pvVertRight (sr, er) top = [er]

-- | Computes all possible merges of two child transitions.
-- Since transitions here only represent the certain edges,
-- 'pvMerge' must also take into account unelaborated edges,
-- which are not present in the child transitions.
pvMerge :: (Ord i, Diatonic i) => Merge (Edges i) (Notes i) (PVLeftMost i)
pvMerge notesl (Edges leftTs leftNTs) (Notes notesm) (Edges rightTs rightNTs) notesr is2nd
  = map mkTop combinations
 where
  -- preprocessing of the notes left and right of the merge
  innerL  = T <$> innerNotes notesl
  innerR  = T <$> innerNotes notesr

  -- find all reduction options for every pitch
  options = noteOptions <$> MS.toOccurList notesm
  noteOptions (note, n)
    | n < MS.size mandatoryLeft || n < MS.size mandatoryRight = []
    | otherwise = partitionEdgeEithers <$> goL mandatoryLeft mandatoryRight n []
   where
    -- compute the mandatory edges for the current pitch:
    mleftTs        = MS.map (T . fst) $ MS.filter ((== Inner note) . snd) leftTs
    mleftNTs       = MS.map (NT . fst) $ MS.filter ((== note) . snd) leftNTs
    mrightTs = MS.map (T . snd) $ MS.filter ((== Inner note) . fst) rightTs
    mrightNTs      = MS.map (NT . snd) $ MS.filter ((== note) . fst) rightNTs
    mandatoryLeft  = mleftTs <> mleftNTs
    mandatoryRight = mrightTs <> mrightNTs

    -- the possible reductions of a (multiple) pitch are enumerated in three stages:

    -- stage 1: consume all mandatory edges on the left
    goL ml mr 0 acc | MS.null ml && MS.null mr = pure acc
                    | otherwise                = []
    goL ml mr n acc = case MS.minView ml of
      Just (l, ml') -> do
        (new, mr') <- pickLeft n l mr
        goL ml' mr' (n - 1) (new : acc)
      Nothing -> goR mr n acc
    -- combine a mandatory left with a mandatory right or free right edge
    pickLeft n l mr | n > MS.size mr = mand <> opt
                    | otherwise      = mand
     where
      mand = do
        r   <- MS.distinctElems mr
        red <- maybeToList $ tryReduction True True l r
        pure (red, MS.delete r mr)
      -- TODO: remove mr options here?
      opt = fmap (, mr) $ catMaybes $ tryReduction True False l <$> innerR

    -- stage 2: consume all remaining mandatory edges on the right
    goR mr 0 acc | MS.null mr = pure acc
                 | otherwise  = []
    goR mr n acc = case MS.minView mr of
      Just (r, mr') -> do
        new <- pickRight r
        goR mr' (n - 1) (new : acc)
      Nothing -> goFree n acc
    -- combine mandatory right with free left edge
    pickRight r = catMaybes $ (\l -> tryReduction False True l r) <$> innerL

    -- stage 3: explain all remaining notes through a combination of unknown edges
    goFree 0 acc = pure acc
    goFree n acc = do
      new <- pickFree
      goFree (n - 1) (new : acc)
    -- combine two free edges
    pickFree = do
      l <- innerL
      r <- innerR
      maybeToList $ tryReduction False False l r

    -- at all stages: try out potential reductions
    -- two terminal edges: any ornament
    tryReduction lchild rchild (T notel) (T noter) = do
      (orn, parent) <- findOrnament notel (Inner note) noter
      pure $ case parent of
        (T  parent) -> T (parent, (note, orn, lchild, rchild))
        (NT parent) -> NT (parent, (note, lchild, rchild))
    -- a non-terminal edge left and a terminal edge right: passing note
    tryReduction lchild rchild notel@(NT _) noter@(T _) = do
      parent <- findPassing notel note noter
      pure $ NT (parent, (note, lchild, rchild))
    -- a terminal edge left and a non-terminal edge right: passing note
    tryReduction lchild rchild notel@(T _) noter@(NT _) = do
      parent <- findPassing notel note noter
      pure $ NT (parent, (note, lchild, rchild))
    -- all other combinations are forbidden
    tryReduction _ _ _ _ = Nothing

  -- compute all possible combinations of reduction options
  combinations = if any L.null options     -- check if any note has no options
    then []                                -- if yes, then no reduction is possible at all
    else foldM pickOption ([], []) options -- otherwise, compute all combinations
  -- picks all different options for a single note in the list monad
  pickOption (accT, accNT) opts = do
    (ts, nts) <- opts
    pure (ts <> accT, nts <> accNT)

  -- convert a combination into a derivation operation:
  -- pick the left-most derivation operation (depending on the left child edge)
  op = if is2nd then LMSplitRight else LMSplitLeft
  -- turn the accumulated information into the format expected from the evaluator
  mkTop (ts, nts) =
    ( Edges (MS.fromList (fst <$> ts)) (MS.fromList (fst <$> nts))
    , op $ SplitOp tmap ntmap
    )
   where
    tmap  = M.fromListWith (<>) $ fmap (: []) <$> ts
    ntmap = M.fromListWith (<>) $ fmap (: []) <$> nts

-- | Computes all potential ways a surface transition could have been frozen.
-- In this grammar, this operation is unique and just turns ties into edges.
pvThaw
  :: (Foldable t, Ord i)
  => StartStop (Notes i)
  -> t (Edge i)
  -> StartStop (Notes i)
  -> [(Edges i, PVLeftMost i)]
pvThaw l e r = [(Edges (MS.fromList $ toList e) MS.empty, LMFreeze FreezeOp)]

pvSlice
  :: (Foldable t, Interval i, Ord (ICOf i)) => t (Pitch i) -> Notes (ICOf i)
pvSlice = Notes . MS.fromList . fmap pc . toList

-- evaluators in specific semirings
-- ================================

pvDeriv
  :: (Foldable t, Ord i, Diatonic i, i ~ ICOf i', Foldable t2, Interval i')
  => Eval
       (Edges i)
       (t (Edge i))
       (Notes i)
       (t2 (Pitch i'))
       (Derivations (PVLeftMost i))
pvDeriv = mapEvalScore Do protoVoiceEvaluator

pvCount''
  :: (Foldable t, Foldable t2, Interval i, Ord (ICOf i), Diatonic (ICOf i))
  => Eval
       (Edges (ICOf i))
       (t (Edge (ICOf i)))
       (Notes (ICOf i))
       (t2 (Pitch i))
       Int
pvCount'' = mapEvalScore (const 1) protoVoiceEvaluator

pvCount'
  :: (Foldable t, Foldable t2, Ord (ICOf i), Diatonic (ICOf i), Interval i)
  => Eval
       (RightBranchHori, Edges (ICOf i))
       (t (Edge (ICOf i)))
       ((), Notes (ICOf i))
       (t2 (Pitch i))
       Int
pvCount' = rightBranchHori pvCount''

pvCount
  :: (Foldable t, Foldable t2, Ord (ICOf i), Diatonic (ICOf i), Interval i)
  => Eval
       (Merged, (RightBranchHori, Edges (ICOf i)))
       (t (Edge (ICOf i)))
       ((), ((), Notes (ICOf i)))
       (t2 (Pitch i))
       Int
pvCount = splitFirst pvCount'
