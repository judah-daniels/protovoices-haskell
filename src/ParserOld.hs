{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module ParserOld where

import           Data.Foldable                  ( find
                                                , foldl'
                                                )
import           Data.List                      ( intersperse
                                                , nub
                                                )
import qualified Data.Map.Strict               as M
import qualified Data.HashMap.Strict           as HM
import qualified Data.Hashable                  ( Hashable )
import           Data.Maybe                     ( listToMaybe
                                                , catMaybes
                                                )
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Q
import qualified Data.Semiring                 as R
import qualified Data.Set                      as S

import qualified Algebra.Graph.Class           as G
import qualified Algebra.Graph.AdjacencyMap    as GA

import           Musicology.Core               as MT

import           Control.DeepSeq
import           Control.Monad                  ( foldM )
import           Control.Parallel
import           Control.Parallel.Strategies
                                         hiding ( Eval(..) )
import           GHC.Generics                   ( Generic )

import qualified Debug.Trace                   as DT

import           Common                         ( StartStop(..) )

-- tracing
-- =======

-- enables/disables tracing
shouldTrace = False

traceIf str x = if shouldTrace then DT.trace str x else x

traceSChart desc n sc =
  pseq sc $ traceIf (desc {-<> ": " <> (show $ scItemsInLayer sc n)-}
                         ) sc

traceTChart desc n tc =
  pseq tc $ traceIf (desc {-<> ": " <> (show $ tcItemsInLayer tc n)-}
                         ) tc

traceTrans k l m = id -- traceFoldable ("T " <> show k <> " " <> show l <> " " <> show m)

traceFoldable desc a = a -- traceIf (desc <> "\n" <> showFoldable a) a

showFoldable a = foldMap showItem a where showItem i = "- " <> show i <> "\n"

-- Slice
--------

-- | A slice of simultaneous musical objects (e.g. pitches).
-- Covers the range [@sFirst@,@sLast@] on the musical surface.
data Slice a = Slice { sFirst :: Int
                     , sLast :: Int
                     , sContent :: StartStop a
                     }
  deriving (Ord, Eq, Generic)
-- TODO: change to (start,length) indices?

-- some instances for Slice

instance (NFData a) => NFData (Slice a)

instance (Show a) => Show (Slice a) where
  show (Slice f l c) = show f <> "," <> show c <> "," <> show l

-- | Return the length of a slice.
sliceLen :: Slice a -> Int
sliceLen (Slice f l _) = l - f + 1

-- Transition
-------------

-- | A transition between two slices.
-- Encodes the relationships between the musical objects in two adjacent slices.
-- @e@ is the type of the transition content (e.g. set of edges).
-- @a@ is the type of the slice content (e.g. set of pitches).
-- Transitions can connect slices that are not adjacent on the surface,
-- if the content between the slices has been reduced away during parsing.
-- Since the transition depends on the content of the slices it connects,
-- the slices are represented completely and not just by index.
data Transition e a = Transition { tLeftSlice  :: Slice a
                                 , tRightSlice    :: Slice a
                                 , tEdges :: e
                                 }
  deriving (Ord, Eq, Generic)

-- some instances for transitions

instance (NFData a, NFData e) => NFData (Transition e a)

instance (Show e, Show a) => Show (Transition e a) where
  show (Transition f t e) = show f <> "-(" <> show e <> ")-" <> show t

-- Items
--------

-- | A chart item.
-- Combines an item (usually a slice or transition) with a semiring value.
data Item i v = (:=) { iItem :: i
                     , iValue :: v
                     }
  deriving (Ord, Eq, Generic)

-- some instances for items

instance (NFData i, NFData v) => NFData (Item i v)

instance (Show i, Show v) => Show (Item i v) where
  show (i := v) = "[" <> show i <> "]=" <> show v

-- | A slice item.
type SItem a v = Item (Slice a) v

-- | A transition item.
type TItem e a v = Item (Transition e a) v

-- slice and transition charts
-- ===========================

-- slice chart
--------------

-- | A slice chart.
-- Stores slice items by pair indices @(at,n)@,
-- where @at@ is the offset on the surface.
-- and @n@ is the length of the slice wrt. the surface.
-- Each such pair index contains several items, but items with the same content
-- are grouped and their values are added in the respective semiring.
newtype SChart a v = SChart (HM.HashMap (Int,Int) (M.Map (Slice a) v))
  deriving (Generic)

instance (NFData a, NFData v) => NFData (SChart a v)

-- | The empty slice chart.
scEmpty :: SChart a v
scEmpty = SChart mempty

-- | Creates a slice chart from a collection of slice items.
scFromFoldable
  :: (R.Semiring v, Eq a, Ord a, Foldable t) => t (SItem a v) -> SChart a v
scFromFoldable = scMerge scEmpty

-- | Merges a collection of slice items into a slice chart,
-- adding the values of duplicate items.
scMerge
  :: (R.Semiring v, Foldable t, Eq a, Ord a)
  => SChart a v
  -> t (SItem a v)
  -> SChart a v
scMerge (SChart chart) ss = SChart $ foldl' merge chart ss
 where
   -- insertWith inserts items into the outer map at a certain position
   -- unionWith adds the values of identical slices at that position
  merge chart (s := v) = HM.insertWith (M.unionWith R.plus)
                                       (sFirst s, sliceLen s)
                                       mItem
                                       chart
    where mItem = M.singleton s v

-- | Returns the list of slice items starting at @at@ with length @len@.
scGet :: SChart a v -> Int -> Int -> [SItem a v]
scGet (SChart chart) at len = fmap mkItem $ M.toList $ HM.lookupDefault
  M.empty
  (at, len)
  chart
  where mkItem (s, v) = s := v

scGetVal :: (R.Semiring v, Ord a) => SChart a v -> Int -> Int -> Slice a -> v
scGetVal (SChart chart) at len slice = M.findWithDefault R.zero slice cell
  where cell = HM.lookupDefault M.empty (at, len) chart

-- | The number of items in layer @n@ (i.e. with length @n@).
scItemsInLayer :: SChart a v -> Int -> Int
scItemsInLayer (SChart chart) n =
  sum $ length <$> HM.filterWithKey inLayer chart
  where inLayer (_, len) _ = len == n

-- transition chart
-------------------

-- | A transition chart.
-- Stores transitions by tuple indices @(at,lenl,skip,lenr)@.
-- As transitions depend on the slices they connect,
-- @at@ is the offset index of the left slice,
-- @lenl@ is the length of the left slice,
-- @skip@ is the number of indices skipped by the transition,
-- and @lenr@ is the length of the right slice.
-- Each tuple index usually points to several items, but items with the same content
-- are grouped and their values are added in the respective semiring.
newtype TChart e a v = TChart (HM.HashMap (Int,Int,Int,Int) (M.Map (Transition e a) v))
  deriving (Generic)

instance (NFData e, NFData a, NFData v) => NFData (TChart e a v)

-- | The empty transition chart.
tcEmpty :: TChart e a v
tcEmpty = TChart HM.empty

-- | Creates a transition chart from a collection of items.
tcFromFoldable
  :: (R.Semiring v, Ord e, Ord a, Foldable t) => t (TItem e a v) -> TChart e a v
tcFromFoldable = tcMerge tcEmpty

-- | Merges a collection of transition items into a transition chart,
-- adding the values of duplicate items.
tcMerge
  :: (R.Semiring v, Foldable t, Ord e, Ord a)
  => TChart e a v
  -> t (TItem e a v)
  -> TChart e a v
tcMerge (TChart tc) ts = TChart $ foldl' merge tc ts
 where
  -- insertWith inserts items into the outer chart at a certain position
  -- unionWith adds the values of identical transitions at that position
  merge chart (t@(Transition from to _) := v) = HM.insertWith
    (M.unionWith R.plus)
    (at, lenl, skip, lenr)
    hmItem
    chart
   where
    at     = sFirst from
    skip   = sFirst to - sLast from - 1
    lenl   = sliceLen from
    lenr   = sliceLen to
    hmItem = M.singleton t v

-- | Returns the list of transition items starting at @at@,
-- with skip @k@, left slice length @l@, and right slice length @m@.
tcGet :: TChart e a v -> Int -> Int -> Int -> Int -> [TItem e a v]
tcGet (TChart chart) at lenl skip lenr =
  fmap mkItem $ M.toList $ HM.lookupDefault M.empty (at, lenl, skip, lenr) chart
  where mkItem (a, v) = a := v

-- | Returns the list of goal transitions,
-- i.e. all transitions between ⋊ and ⋉,
-- skipping the whole input of length @len@.
tcGoals :: TChart e a v -> Int -> [TItem e a v]
tcGoals chart len = tcGet chart 0 1 (len - 2) 1

-- | Counts the number of items in layer @n@
-- (i.e. where @skip==n@ or @lenl==n@ or @lenr==n@).
tcItemsInLayer :: TChart e a v -> Int -> Int
tcItemsInLayer (TChart chart) n =
  sum $ length <$> HM.filterWithKey inLayer chart
  where inLayer (_, lenl, skip, lenr) v = lenl == n || skip == n || lenr == n

-- parsing machinery
-- =================

-- evaluators
-------------
-- TODO: return items?

-- | An evaluator for verticalizations.
-- Returns possible verticalizations of a given slice-transition-slice triple.
type EvalVert e a v = Slice a -> Transition e a -> Slice a -> [(Slice a, v)]

-- | An evaluator for transition inference.
-- For a given pair of adjacent slices, returns all possible transitions between them.
type EvalInfer e a v = Slice a -> Slice a -> [(Transition e a, v)]

-- | An evaluator for merges.
-- Returns possible merges of a given transition-slice-transition triple.
type EvalMerge e a v
  = Transition e a -> Slice a -> Transition e a -> [(Transition e a, v)]

-- | A combined evaluator for verticalizations, merges, and transition inference.
-- Additionally, contains a function for mapping terminal slices to semiring values.
data Eval e a v = Eval { evalVert  :: EvalVert e a v
                       , evalInfer :: EvalInfer e a v
                       , evalMerge :: EvalMerge e a v
                       , evalTermSlice  :: Slice a -> v}

-- inference (using evaluators)
-------------------------------

-- | Collects all verticalizations in layer @len@.
-- Uses the vert evaluator @vert@.
-- @inputLen@ is the length of the input.
-- Returns the slice chart with new slices added.
vertSlices
  :: (R.Semiring v, Eq a, Show a, Show v, Ord a)
  => EvalVert e a v
  -> Int
  -> SChart a v
  -> TChart e a v
  -> Int
  -> [SItem a v]
vertSlices vert inputLen schart tchart len =
  traceFoldable ("S " <> show len) $ do
    at   <- [0 .. inputLen - len]  -- offset
    lenl <- [1 .. len - 1]     -- size of first slice
    lenr <- [1 .. len - lenl]     -- size of second slice
    let skip = len - lenl - lenr -- skip of the transition
        ts   = tcGet tchart at lenl skip lenr
    -- select the transition
    (t := tv) <- ts
    -- get the slices from the selected transition
    let sl   = tLeftSlice t
        sr   = tRightSlice t
        slv  = scGetVal schart at lenl sl
        srv  = scGetVal schart (at + len - lenr) lenr sr
        vrhs = slv R.* tv R.* srv -- value of right-hand side
    (\(s, v) -> s := (v R.* vrhs)) <$> vert sl t sr

-- | Infers all adjacent transitions (@skip=0@)
-- with left slice lenght @l@ and right slice length @m@.
-- Uses inference evaluator @inf@.
-- @len@ is the length of the input.
-- Returns a list of new transitions.
inferTrans
  :: (R.Semiring v, Eq a, Eq e, Show e, Show a, Show v)
  => EvalInfer e a v
  -> Int
  -> SChart a v
  -> TChart e a v
  -> Int
  -> Int
  -> [TItem e a v]
inferTrans inf inputLen schart tchart lenl lenr = traceTrans 0 lenl lenr $ do
  at <- [0 .. inputLen - (lenl + lenr)]
  let s1s = scGet schart at lenl
      s2s = scGet schart (at + lenl) lenr
  (s1 := _) <- s1s
  (s2 := _) <- s2s
  uncurry (:=) <$> inf s1 s2

-- TODO: check if some transitions or slices can be selected directly
-- instead of running over all combinations
-- | Collects all merges of transitions skipping @skip@ indices,
-- with left slice length @lenl@ and right slice length @lenr@.
-- Uses merge evaluator @merge@.
-- @inputLen@ is the length of the input.
-- Returns a list of merged transitions.
mergeTrans
  :: (R.Semiring v, Eq a, Eq e, Ord a, Show e, Show a, Show v)
  => EvalMerge e a v
  -> Int
  -> SChart a v
  -> TChart e a v
  -> Int
  -> Int
  -> Int
  -> [TItem e a v]
mergeTrans merge inputLen schart tchart skip lenl lenr =
  traceTrans skip lenl lenr $ do
    at     <- [0 .. inputLen - skip]
    skipl  <- [0 .. skip - 1]
    lenmid <- [1 .. skip - skipl]
    let skipr = skip - skipl - lenmid
        at2   = at + lenl + skipl
        t1s   = tcGet tchart at lenl skipl lenmid
        t2s   = tcGet tchart at2 lenmid skipr lenr
    -- select both transitions
    (t1 := t1v) <- t1s
    (t2 := t2v) <- t2s
    -- TODO: can this be avoided by selecting the slice first and quering the transitions?
    if tRightSlice t1 == tLeftSlice t2
      then do
        let s    = tRightSlice t1
            sv   = scGetVal schart at2 lenmid s
            vrhs = t1v R.* sv R.* t2v
        (\(t, v) -> t := (v R.* vrhs)) <$> merge t1 s t2
      else []

-- main parsing loop
--------------------

-- TODO (meta): can we simplify the order of merging if in step n
-- instead of skip==lenl==lenr==n we require only skip+lenl+lenr=n?
-- | Parses layer @n@ of the slice and transition charts,
-- i.e. \(S_n\) and \(T_{nnn}\).
-- @eval@ is the combined evaluator and @inputLen@ is the length of the input.
-- Returns the pair of slice and transition chart
-- and can hence be used to fold over a list of @n@s.
parseStep
  :: ( R.Semiring v
     , Eq a
     , Eq e
     , Ord e
     , Ord a
     , Show a
     , Show e
     , Show v
     , NFData e
     , NFData a
     , NFData v
     )
  => Eval e a v
  -> Int
  -> (SChart a v, TChart e a v)
  -> Int
  -> (SChart a v, TChart e a v)
parseStep eval inputLen (schart, tchart) n =
  ( traceSChart ("SChart " <> show n) n schart' -- updated slice chart
  , traceTChart ("TChart " <> show n) n tchart' -- updated transition chart
  )
 where
  -- Step 1: Add new slices (verticalization).
  schart' =
    scMerge schart $ vertSlices (evalVert eval) inputLen schart tchart n

  -- Step 2: Infer new edges (with skip=0) with the new slices to the left/right.
  tchart0 =
    traceTChart ("TChart (inf) " <> show n) n $ tcMerge tchart $ left <> right
   where
    -- helper: infers new transitions based on size of left and right slice
    inf  = inferTrans (evalInfer eval) inputLen schart' tchart
    -- new transitions left of new slices (other slice length from 1 to n)
    left = do
      lenl <- [1 .. n]
      inf lenl n
    -- new transitions right of new slices (other slice length from 1 to n-1)
    right = do
      lenr <- [1 .. n - 1]
      inf n lenr

  -- Step 3: Merge transition for new slices (lenl <= n, lenr <= n, skip <= n).
  --         We already have lenl,lenr,skip <= n-1,
  --         so we just need the cases where one of lenl, lenr, and skip == n.
  --         We fix each of them in turn and iterate over the other two.
  --         A fixed lenl or lenr with some skip s depends on transitions with skip s-1.
  --         Similarly, skip=lenl=lenr=n depends on lenl=lenr=n for skip up to n-1.
  --         Therfore, the order of evaluation is fixed and new transitions need to be passed on.

  -- Step 3.1: fixed sides (iterate skip from 1 to n-1)
  tcFixedSides = foldl' doSides tchart0 [1 .. n - 1]
   where
      -- for each skip merge the fixed left and fixed right side
    doSides tc skip = tcMerge tc $ leftFix <> rightFix
     where
        -- helper: merges transitions for fixed skip and given lenl and lenr
      merge   = mergeTrans (evalMerge eval) inputLen schart' tc skip
      -- fixed left: iterate over right
      leftFix = do
        lenr <- [1 .. n]
        merge n lenr
      -- fixed right: iterate over left (lenl==lenr==n is covered in leftFix)
      rightFix = do
        lenl <- [1 .. n - 1]
        merge lenl n

  -- Step 3.2: fixed skip (depends on fixed sides)
  tchart' = tcMerge tcFixedSides $ do
    lenl <- [1 .. n] -- iterate over left
    lenr <- [1 .. n] -- iterate over right
    mergeTrans (evalMerge eval) inputLen schart' tcFixedSides n lenl lenr

-- | The main entry point to parsing.
-- Takes an evaluator and a sequence of slice contents.
-- Returns a semiring value.
parse
  :: ( R.Semiring v
     , Eq a
     , Eq e
     , Ord a
     , Ord e
     , Show v
     , Show a
     , Show e
     , NFData e
     , NFData a
     , NFData v
     )
  => Eval e a v
  -> [a]
  -> v
parse eval inSlices = R.sum $ iValue <$> tcGoals tfinal totalLen
 where
  inputLen = length inSlices
  totalLen = inputLen + 2 -- includes ⋊ and ⋉
  slices   = (:⋊) : (Inner <$> inSlices) <> [(:⋉)]
  mkSlice a i = s := evalTermSlice eval s where s = Slice i i a
  sinit = scFromFoldable $ zipWith mkSlice slices [0 ..]
  inf0 =
    tcFromFoldable $ inferTrans (evalInfer eval) totalLen sinit tcEmpty 1 1
  tinit = tcMerge inf0 $ mergeTrans (evalMerge eval) totalLen sinit inf0 1 1 1
  (sfinal, tfinal) =
    foldl' (parseStep eval totalLen) (sinit, tinit) [2 .. inputLen]



-- useful semirings
-- ================

-- derivation
-------------

-- | Represents a derivation operation in terms of slices and transitions.
-- Since the indicies of transitions and slices are included,
-- a derivation step is independent of its context (such as a strict leftmost-derivation order).
data DerivStep e a = Split (Transition e a) (Transition e a) (Slice a) (Transition e a)
                   | Horizontalize (Slice a) (Slice a) (Transition e a) (Slice a)
                   | EmitTrans (Transition e a)
                   | EmitSlice (Slice a)
  deriving (Eq, Ord, Generic)

instance (NFData e, NFData a) => NFData (DerivStep e a)

instance (Show e, Show a) => Show (DerivStep e a) where
  show (Split h t1 s t2) =
    "Split: ["
      <> show h
      <> "] -> ["
      <> show t1
      <> "] ["
      <> show s
      <> "] ["
      <> show t2
      <> "]"
  show (Horizontalize h s1 t s2) =
    "Horizontalize: ["
      <> show h
      <> "] -> ["
      <> show s1
      <> "] ["
      <> show t
      <> "] ["
      <> show s2
      <> "]"
  show (EmitTrans t) = "EmitTrans: [" <> show t <> "]"
  show (EmitSlice s) = "EmitSlice: [" <> show s <> "]"

-- use the semiring instance of Set, where plus is union
-- and times mappends (here: concats) all elements pairwise
-- | The derivation semiring.
type Derivations e a = (S.Set [DerivStep e a])

-- unfinished
-- ==========

-- viterbi stuff
----------------

newtype Viterbi n = Vit n

instance (Num n, Ord n) => R.Semiring (Viterbi n) where
  zero = Vit 0
  one  = Vit 1
  plus (Vit v1) (Vit v2) = Vit $ max v1 v2
  times (Vit v1) (Vit v2) = Vit $ v1 * v2

-- "first" semiring
-------------------

data First a = FirstIs a
             | NoneSoFar

instance (Monoid a) => R.Semiring (First a) where
  zero = NoneSoFar
  one  = FirstIs mempty
  plus NoneSoFar     b = b
  plus a@(FirstIs _) _ = a
  times NoneSoFar   _           = NoneSoFar
  times _           NoneSoFar   = NoneSoFar
  times (FirstIs a) (FirstIs b) = FirstIs (a <> b)
