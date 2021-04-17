{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module PVGrammar.Parse where

import           Common
import           Display
import           PVGrammar

import           Musicology.Pitch

import qualified Data.Set                      as S
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.MultiSet                 as MS
import           Data.Foldable                  ( toList
                                                , foldl'
                                                )
import           Data.Maybe                     ( catMaybes
                                                , maybeToList
                                                )
import           Control.Monad                  ( foldM )
import           Debug.Trace                    ( traceId
                                                , traceShow
                                                , traceShowId
                                                , trace
                                                )

-- helper type: Either for terminal and non-terminal edges
-- -------------------------------------------------------

-- | A tag that distinguishes between objects related to terminal and non-terminal edges.
-- Like 'Either', but with semantic constructor names to avoid confusion.
data EdgeEither a b = T !a  -- ^ marks an terminal edge (or some related object)
                    | NT !b -- ^ marks a non-terminal edge (or some related object)
  deriving (Eq, Ord, Show)

-- | Separates 'T's and 'NT's in a collection into a pair of lists for each type.
-- Analogous to 'partitionEithers'.
partitionEdgeEithers :: Foldable t => t (EdgeEither a b) -> ([a], [b])
partitionEdgeEithers = foldl' select ([], [])
 where
  select (ts, nts) (T  t ) = (t : ts, nts)
  select (ts, nts) (NT nt) = (ts, nt : nts)

-- helper type: enum for possible operations
-- -----------------------------------------

-- | A tag that distinguishes four different types of operations:
--  terminal split, non-termal split, left ornament, and right ornament
data Elaboration a b c d = ET !a  -- ^ marks a terminal split
                           | EN !b -- ^ marks a non-terminal split
                           | ER !c  -- ^ marks a right ornament
                           | EL !d  -- ^ marks a left ornament
  deriving (Eq, Ord, Show)

partitionElaborations
  :: Foldable t => t (Elaboration a b c d) -> ([a], [b], [c], [d])
partitionElaborations = foldl' select ([], [], [], [])
 where
  select (a, b, c, d) (ET t) = (t : a, b, c, d)
  select (a, b, c, d) (EN n) = (a, n : b, c, d)
  select (a, b, c, d) (ER l) = (a, b, l : c, d)
  select (a, b, c, d) (EL r) = (a, b, c, r : d)

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
  -> Bool
  -> Bool
  -> Maybe (Ornament, EdgeEither (Edge i) (InnerEdge i))
findOrnament (Inner l) (Inner m) (Inner r) True True
  | l == m && m == r = Just (FullRepeat, T (Inner l, Inner r))
  | l == m && so     = Just (RightRepeatOfLeft, T (Inner l, Inner r))
  | m == r && so     = Just (LeftRepeatOfRight, T (Inner l, Inner r))
  | s1 && so         = Just (RightNeighborOfLeft, T (Inner l, Inner r))
  | s2 && so         = Just (LeftNeighborOfRight, T (Inner l, Inner r))
 where
  s1 = isStep $ l `pto` m
  s2 = isStep $ m `pto` r
  so = isStep $ l `pto` r
findOrnament (Inner l) (Inner m) (Inner r) _ _
  | l == r && s1              = Just (FullNeighbor, T (Inner l, Inner r))
  | s1 && s2 && between l m r = Just (Passing, NT (l, r))
 where
  s1 = isStep $ l `pto` m
  s2 = isStep $ m `pto` r
-- findOrnament (:⋊) (Inner m) (Inner r)
--   | m == r             = Just (LeftRepeatOfRight, T ((:⋊), Inner r))
--   | isStep $ m `pto` r = Just (LeftNeighborOfRight, T ((:⋊), Inner r))
-- findOrnament (Inner l) (Inner m) (:⋉)
--   | l == m             = Just (RightRepeatOfLeft, T (Inner l, (:⋉)))
--   | isStep $ l `pto` m = Just (RightNeighborOfLeft, T (Inner l, (:⋉)))
findOrnament (:⋊) (Inner _) (:⋉) _ _ = Just (RootNote, T ((:⋊), (:⋉)))
findOrnament _    _         _    _ _ = Nothing

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

findRightOrnament
  :: (Eq i, Diatonic i) => Pitch i -> Pitch i -> Maybe RightOrnament
findRightOrnament l m | l == m             = Just SingleRightRepeat
                      | isStep (l `pto` m) = Just SingleRightNeighbor
                      | otherwise          = Nothing

findLeftOrnament
  :: (Eq i, Diatonic i) => Pitch i -> Pitch i -> Maybe LeftOrnament
findLeftOrnament m r | m == r             = Just SingleLeftRepeat
                     | isStep (m `pto` r) = Just SingleLeftNeighbor
                     | otherwise          = Nothing

-- evaluator interface
-- ===================

-- | The evaluator that represents the proto-voice grammar.
-- As scores it returns a representation of each operation.
-- These scores do not form a semiring,
-- but can be embedded into different semirings using 'evalMapScores'.
protoVoiceEvaluator
  :: ( Foldable t
     , Eq (ICOf i)
     , Ord (ICOf i)
     , Diatonic (ICOf i)
     , Foldable t2
     , Interval i
     , Notation (Pitch (ICOf i))
     , Show (Pitch (ICOf i))
     )
  => Eval
       (Edges (ICOf i))
       (t (Edge (ICOf i)))
       (Notes (ICOf i))
       (t2 (Pitch i))
       (PVLeftMost (ICOf i))
protoVoiceEvaluator =
  mkLeftmostEval pvVertMiddle pvVertLeft pvVertRight pvMerge pvThaw pvSlice

-- | Computes the verticalization of a middle transition.
-- If the verticalization is admitted, returns the corresponding operation.
pvVertMiddle :: (Eq i, Ord i) => VertMiddle (Edges i) (Notes i) (Hori i)
pvVertMiddle (Notes nl, edges, Notes nr)
  | any notARepetition (edgesT edges) = Nothing
  | otherwise                         = Just (Notes top, op)
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
  op   = HoriOp (left <> right <> both) edges

-- | Computes all left parent transitions for a verticalization and a left child transition.
-- Here, this operation is always admitted and unique,
-- so the edges from the child transition are just passed through.
pvVertLeft :: VertLeft (Edges i) (Notes i)
pvVertLeft (el, sl) top = [el]

-- | Computes all right parent transition for a verticalization and a right child transition.
-- Here, this operation is always admitted and unique,
-- so the edges from the child transition are just passed through.
pvVertRight :: VertRight (Edges i) (Notes i)
pvVertRight (sr, er) top = [er]

-- | Computes all possible merges of two child transitions.
-- Since transitions here only represent the certain edges,
-- 'pvMerge' must also take into account unelaborated edges,
-- which are not present in the child transitions.
pvMerge
  :: (Ord i, Diatonic i, Notation (Pitch i), Show (Pitch i))
  => StartStop (Notes i)
  -> Edges i
  -> Notes i
  -> Edges i
  -> StartStop (Notes i)
  -> [(Edges i, Split i)]
pvMerge notesl (Edges leftTs leftNTs) (Notes notesm) (Edges rightTs rightNTs) notesr
  = map mkTop combinations
 where
  -- preprocessing of the notes left and right of the merge
  !innerL  = T <$> innerNotes notesl
  !innerR  = T <$> innerNotes notesr

  -- find all reduction options for every pitch
  !options = noteOptions <$> MS.toOccurList notesm
  noteOptions (note, n)
    | n < MS.size mandatoryLeft || n < MS.size mandatoryRight = []
    | otherwise = partitionElaborations
      <$> goL mandatoryLeft mandatoryRight n []
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
    pickLeft n l mr | n > MS.size mr = mand <> opt <> single
                    | otherwise      = mand
     where
      mand = do
        r   <- MS.distinctElems mr
        red <- maybeToList $ tryReduction True True l note r
        pure (red, MS.delete r mr)
      -- TODO: remove mr options here?
      opt = fmap (, mr) $ catMaybes $ tryReduction True False l note <$> innerR
      single = fmap (, mr) $ maybeToList $ tryLeftReduction True note l

    -- stage 2: consume all remaining mandatory edges on the right
    goR mr 0 acc | MS.null mr = pure acc
                 | otherwise  = []
    goR mr n acc = case MS.minView mr of
      Just (r, mr') -> do
        new <- pickRight r
        goR mr' (n - 1) (new : acc)
      Nothing -> goFree freeOptions n acc
    -- combine mandatory right with free left edge
    pickRight r = opt <> single
     where
      opt    = catMaybes $ (\l -> tryReduction False True l note r) <$> innerL
      single = maybeToList $ tryRightReduction True note r

    -- stage 3: explain all remaining notes through a combination of unknown edges
    goFree _            0 acc = pure acc
    goFree []           _ _   = []
    goFree [lastOpt   ] n acc = pure $ L.replicate n lastOpt <> acc
    goFree (opt : opts) n acc = do
      nopt <- [0 .. n]
      goFree opts (n - nopt) (L.replicate nopt opt <> acc)
    -- list all options for free reduction
    freeOptions  = pickFreeBoth <> pickFreeLeft <> pickFreeRight
    -- combine two free edges
    pickFreeBoth = do
      l <- innerL
      r <- innerR
      maybeToList $ tryReduction False False l note r
    -- reduce to left using free edge
    pickFreeLeft  = catMaybes $ tryLeftReduction False note <$> innerL
    -- reduce to right using free edge
    pickFreeRight = catMaybes $ tryRightReduction False note <$> innerR

  -- at all stages: try out potential reductions:

  -- two terminal edges: any ornament
  tryReduction lIsUsed rIsUsed (T notel) notem (T noter) = do
    (orn, parent) <- findOrnament notel (Inner notem) noter lIsUsed rIsUsed
    pure $ case parent of
      (T  parent) -> ET (parent, (notem, orn, lIsUsed, rIsUsed))
      (NT parent) -> EN (parent, (notem, lIsUsed, rIsUsed))
  -- a non-terminal edge left and a terminal edge right: passing note
  tryReduction lIsUsed rIsUsed notel@(NT _) notem noter@(T _) = do
    parent <- findPassing notel notem noter
    pure $ EN (parent, (notem, lIsUsed, rIsUsed))
  -- a terminal edge left and a non-terminal edge right: passing note
  tryReduction lIsUsed rIsUsed notel@(T _) notem noter@(NT _) = do
    parent <- findPassing notel notem noter
    pure $ EN (parent, (notem, lIsUsed, rIsUsed))
  -- all other combinations are forbidden
  tryReduction _ _ _ _ _ = Nothing

  -- single reduction to a left parent
  tryLeftReduction isUsed notem (T (Inner notel)) = do
    orn <- findRightOrnament notel notem
    pure $ ER (notel, (notem, orn, isUsed))
  tryLeftReduction _ _ _ = Nothing

  -- single reduction to a right parent
  tryRightReduction isUsed notem (T (Inner noter)) = do
    orn <- findLeftOrnament notem noter
    pure $ EL (noter, (notem, orn, isUsed))
  tryRightReduction _ _ _ = Nothing

  -- compute all possible combinations of reduction options
  !combinations = if any L.null options     -- check if any note has no options
    then []                                -- if yes, then no reduction is possible at all
    else foldM pickOption ([], [], [], []) options -- otherwise, compute all combinations
  -- picks all different options for a single note in the list monad
  pickOption (accT, accNT, accL, accR) opts = do
    (ts, nts, ls, rs) <- opts
    pure (ts <> accT, nts <> accNT, ls <> accL, rs <> accR)

  -- convert a combination into a derivation operation:
  -- turn the accumulated information into the format expected from the evaluator
  mkTop (ts, nts, rs, ls) = if True -- validate
    then (top, SplitOp tmap ntmap rmap lmap)
    else
      error
      $  "invalid merge:\n  notesl="
      <> show notesl
      <> "\n  notesr="
      <> show notesr
      <> "\n  notesm="
      <> show (Notes notesm)
      <> "\n  left="
      <> show (Edges leftTs leftNTs)
      <> "\n  right="
      <> show (Edges rightTs rightNTs)
      <> "\n  top="
      <> show top
   where
    validate =
      all ((`L.elem` innerNotes notesl) . fst . fst) ts
        && all ((`L.elem` innerNotes notesr) . snd . fst)   ts
        && all ((`L.elem` innerNotes notesl) . Inner . fst) rs
        && all ((`L.elem` innerNotes notesr) . Inner . fst) ls
    mapify xs = M.fromListWith (<>) $ fmap (: []) <$> xs
    tmap  = mapify ts
    ntmap = mapify nts
    lmap  = mapify ls
    rmap  = mapify rs
    top   = Edges (MS.fromList (fst <$> ts)) (MS.fromList (fst <$> nts))

-- | Computes all potential ways a surface transition could have been frozen.
-- In this grammar, this operation is unique and just turns ties into edges.
pvThaw
  :: (Foldable t, Ord i)
  => StartStop (Notes i)
  -> Maybe (t (Edge i))
  -> StartStop (Notes i)
  -> [(Edges i, Freeze)]
pvThaw l e r = [(Edges (MS.fromList $ maybe [] toList e) MS.empty, FreezeOp)]

pvSlice
  :: (Foldable t, Interval i, Ord (ICOf i)) => t (Pitch i) -> Notes (ICOf i)
pvSlice = Notes . MS.fromList . fmap pc . toList

-- evaluators in specific semirings
-- ================================

protoVoiceEvaluator'
  :: ( Foldable t
     , Eq (ICOf i)
     , Ord (ICOf i)
     , Diatonic (ICOf i)
     , Foldable t2
     , Interval i
     , Notation (Pitch (ICOf i))
     , Show (Pitch (ICOf i))
     )
  => Eval
       (Edges (ICOf i))
       (t (Edge (ICOf i)))
       (Notes (ICOf i))
       (t2 (Pitch i))
       (PVLeftMost (ICOf i))
protoVoiceEvaluator' = Eval vm vl vr filterSplit t s
 where
  (Eval vm vl vr mg t s) = protoVoiceEvaluator
  filterSplit l lt mid rt r typ = filter ok $ mg l lt mid rt r typ
  ok (_, LMSplitLeft op    ) = not $ onlyRepeats op
  ok (_, LMSplitLeftOnly op) = not $ onlyRepeats op
  ok (_, LMSplitRight op   ) = not $ onlyRepeats op
  ok _                       = False
  onlyRepeats op@(SplitOp ts nts rs ls) =
    M.null nts && (allRepetitionsLeft || allRepetitionsRight)
   where
    allSinglesRepeat = all (checkS (== SingleRightRepeat)) (M.toList rs)
      && all (checkS (== SingleLeftRepeat)) (M.toList ls)
    allRepetitionsLeft =
      all (checkD isRepetitionOnLeft) (M.toList ts) && allSinglesRepeat
    allRepetitionsRight =
      all (checkD isRepetitionOnRight) (M.toList ts) && allSinglesRepeat
  checkD pred (_, os) = all (pred . (\(_, o, _, _) -> o)) os
  checkS pred (_, os) = all (pred . (\(_, o, _) -> o)) os

pvDerivUnrestricted
  :: ( Foldable t
     , Foldable t2
     , Ord (ICOf i)
     , Diatonic (ICOf i)
     , Interval i
     , Notation (Pitch (ICOf i))
     , Show (Pitch (ICOf i))
     )
  => Eval
       (Edges (ICOf i))
       (t (Edge (ICOf i)))
       (Notes (ICOf i))
       (t2 (Pitch i))
       (Derivations (PVLeftMost (ICOf i)))
pvDerivUnrestricted = mapEvalScore Do protoVoiceEvaluator'


pvDeriv
  :: ( Foldable t
     , Foldable t2
     , Ord (ICOf i)
     , Diatonic (ICOf i)
     , Interval i
     , Notation (Pitch (ICOf i))
     , Show (Pitch (ICOf i))
     )
  => Eval
       (Merged, (RightBranchHori, Edges (ICOf i)))
       (t (Edge (ICOf i)))
       ((), ((), Notes (ICOf i)))
       (t2 (Pitch i))
       (Derivations (PVLeftMost (ICOf i)))
pvDeriv = splitFirst $ rightBranchHori $ mapEvalScore Do protoVoiceEvaluator'

pvCount''
  :: ( Foldable t
     , Foldable t2
     , Interval i
     , Ord (ICOf i)
     , Diatonic (ICOf i)
     , Notation (Pitch (ICOf i))
     , Show (Pitch (ICOf i))
     )
  => Eval
       (Edges (ICOf i))
       (t (Edge (ICOf i)))
       (Notes (ICOf i))
       (t2 (Pitch i))
       Int
pvCount'' = mapEvalScore (const 1) protoVoiceEvaluator'

pvCount'
  :: ( Foldable t
     , Foldable t2
     , Ord (ICOf i)
     , Diatonic (ICOf i)
     , Interval i
     , Notation (Pitch (ICOf i))
     , Show (Pitch (ICOf i))
     )
  => Eval
       (RightBranchHori, Edges (ICOf i))
       (t (Edge (ICOf i)))
       ((), Notes (ICOf i))
       (t2 (Pitch i))
       Int
pvCount' = rightBranchHori pvCount''

pvCount
  :: ( Foldable t
     , Foldable t2
     , Ord (ICOf i)
     , Diatonic (ICOf i)
     , Interval i
     , Notation (Pitch (ICOf i))
     , Show (Pitch (ICOf i))
     )
  => Eval
       (Merged, (RightBranchHori, Edges (ICOf i)))
       (t (Edge (ICOf i)))
       ((), ((), Notes (ICOf i)))
       (t2 (Pitch i))
       Int
pvCount = splitFirst pvCount'
