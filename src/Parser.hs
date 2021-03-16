{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Parser where

import qualified Scoring                       as S

import qualified Data.Map.Strict               as M
import qualified Data.HashMap.Strict           as HM
import qualified Data.Semiring                 as R

import           Control.Monad.State           as ST

import           GHC.Generics                   ( Generic )
import           Control.DeepSeq
import           Data.Foldable                  ( foldlM
                                                , foldl'
                                                )
import           Data.Maybe                     ( fromMaybe )

-- Basic Types
-- ===========

-- StartStop
------------

-- | A container type that augements the type @a@
-- with symbols for beginning (@:⋊@) and end (@:⋉@).
-- Every other value is wrapped in an @Inner@ constructor.
data StartStop a = (:⋊)
                  | Inner a
                  | (:⋉)
  deriving (Ord, Eq, Generic)

-- some instances for StartStop

instance (NFData a) => NFData (StartStop a)

instance Show a => Show (StartStop a) where
  show (:⋊)      = "⋊"
  show (:⋉)      = "⋉"
  show (Inner a) = show a

instance Functor StartStop where
  fmap f (:⋊)      = (:⋊)
  fmap f (:⋉)      = (:⋉)
  fmap f (Inner a) = Inner $ f a

-- some helper functions for StartStop

-- | From a list of @StartStop@s returns only the elements that are not @:⋊@ or @:⋉@,
-- unwrapped to their original type.
onlyInner :: [StartStop a] -> [a]
onlyInner []              = []
onlyInner (Inner a : rst) = a : onlyInner rst
onlyInner (_       : rst) = onlyInner rst

isInner (Inner a) = True
isInner _         = False

isStart (:⋊) = True
isStart _    = False

isStop (:⋉) = True
isStop _    = False

-- Slices
---------

data Slice a = Slice
  { sFirst   :: Int
  , sContent :: a
  , sLast    :: Int
  }
  deriving (Eq, Ord, Generic)

-- | Return the length of a slice.
sliceLen :: Slice a -> Int
sliceLen (Slice f _ l) = l - f + 1

-- Transitions
--------------

data Transition e a = Transition
  { tLeftSlice  :: Slice a
  , tContent    :: e
  , tRightSlice :: Slice a
  }
  deriving (Eq, Ord, Generic)

transLen :: Transition e a -> Int
transLen (Transition l _ r) = sLast r - sFirst l + 1

-- Items
--------

data Item i v = (:=)
  { iItem :: i
  , iValue :: S.Score v Int
  }

-- | A transition item.
type TItem e a v = Item (Transition e a) v

-- Vert Items

data Vert e a v = Vert
  { vId :: Int
  , vTop :: a
  , vOp :: v
  , vMiddle :: TItem e a v
  }

-- slice and transition charts
-- ===========================

-- ID-generating monad

type WithID a = State Int a

makeId :: WithID Int
makeId = do
  i <- get
  put (i + 1)
  return i

lookupId :: Ord k => k -> M.Map k Int -> WithID (Int, M.Map k Int)
lookupId key idMap = case M.lookup key idMap of
  Just id -> pure (id, idMap)
  Nothing -> do
    id' <- makeId
    pure (id', M.insert key id' idMap)

-- vert chart
-------------

data VCell e a v = VCell
  { vcIDs :: M.Map (a, Slice a, Maybe Int) Int
  , vcLeftMap :: M.Map (Slice a, Maybe Int) [(a, Int)]
  , vcRightMap :: M.Map (Maybe Int, Slice a) [Vert e a v]
  }

vcInsert :: Ord a => VCell e a v -> (a, v, TItem e a v) -> WithID (VCell e a v)
vcInsert (VCell ids leftMap rightMap) (top, op, mid@(tmid := vmid)) = do
  (i, ids') <- lookupId keyIDs ids
  let left'  = M.insertWith (<>) keyLeft [(top, i)] leftMap
      right' = M.insertWith (<>) keyRight [Vert i top op mid] rightMap
  pure $ VCell ids' left' right'
 where
  keyIDs   = (top, tLeftSlice tmid, S.leftSide vmid)
  keyLeft  = (tLeftSlice tmid, S.leftSide vmid)
  keyRight = (S.rightSide vmid, tRightSlice tmid)

vcNew :: Ord a => (a, v, TItem e a v) -> WithID (VCell e a v)
vcNew (top, op, mid@(tmid := vmid)) = do
  (i, ids) <- lookupId keyIDs mempty
  let left  = M.singleton keyLeft [(top, i)]
      right = M.singleton keyRight [Vert i top op mid]
  pure $ VCell ids left right
 where
  keyIDs   = (top, tLeftSlice tmid, S.leftSide vmid)
  keyLeft  = (tLeftSlice tmid, S.leftSide vmid)
  keyRight = (S.rightSide vmid, tRightSlice tmid)

-- | A verticalization chart.
-- Stores verticalization items by pair indices @(at,n)@,
-- where @at@ is the offset on the surface.
-- and @n@ is the length of the resulting slice (or the verticalized edge) wrt. the surface.
-- Each such pair index contains several items, but items with the same interface
-- are grouped and their values are added in the respective semiring.
data VChart e a v = VChart
  { vcCells :: HM.HashMap (Int,Int) (VCell e a v)
  , vcNextID :: Int
  }

-- | The empty slice chart.
vcEmpty :: VChart a e v
vcEmpty = VChart mempty 0

-- | Creates a slice chart from a collection of slice items.
vcFromFoldable
  :: (R.Semiring v, Eq a, Ord a, Foldable t)
  => t (a, v, TItem e a v)
  -> VChart e a v
vcFromFoldable = vcMerge vcEmpty

-- | Merges a collection of slice items into a slice chart,
-- adding the values of duplicate items.
vcMerge
  :: (R.Semiring v, Foldable t, Eq a, Ord a)
  => VChart e a v
  -> t (a, v, TItem e a v)
  -> VChart e a v
vcMerge (VChart chart nextid) ss = VChart chart' nextid'
 where
  (chart', nextid') = runState (foldlM merge chart ss) nextid
  merge chart new@(_, _, mid@(tmid := _)) = do
    cell' <- case HM.lookup key chart of
      Nothing   -> vcNew new
      Just cell -> vcInsert cell new
    pure $ HM.insert key cell' chart
   where
    at  = sFirst $ tLeftSlice tmid
    n   = transLen tmid
    key = (at, n)

-- | Returns the list of slice items starting at @at@ with length @len@.
vcGetFromLeft
  :: Ord a => VChart e a v -> Int -> Int -> Slice a -> Maybe Int -> [(a, Int)]
vcGetFromLeft (VChart chart _) at len lSlice lSide =
  case HM.lookup (at, len) chart of
    Nothing                  -> []
    Just (VCell _ leftMap _) -> M.findWithDefault [] (lSlice, lSide) leftMap

-- | The number of items in layer @n@ (i.e. with length @n@).
scItemsInLayer :: VChart e a v -> Int -> Int
scItemsInLayer (VChart chart _) n =
  sum $ vcLen <$> HM.filterWithKey inLayer chart
 where
  inLayer (_, len) _ = len == n
  vcLen (VCell _ l _) = sum $ length <$> l

-- transition chart
-------------------

type TCell e a v = M.Map (Transition e a, Maybe Int, Maybe Int) (S.Score v Int)

tcInsert
  :: (R.Semiring v, Ord a, Ord e) => TCell e a v -> TItem e a v -> TCell e a v
tcInsert cell (trans := score) = M.insertWith insSafe key score cell
 where
  key = (trans, S.leftSide score, S.rightSide score)
  insSafe new old = fromMaybe old (S.plus new old)

tcNew :: TItem e a v -> TCell e a v
tcNew (trans := score) = M.singleton key score
  where key = (trans, S.leftSide score, S.rightSide score)

-- | A transition chart.
-- Stores transitions by tuple indices @(at,lenl,skip,lenr)@.
-- As transitions depend on the slices they connect,
-- @at@ is the offset index of the left slice,
-- @lenl@ is the length of the left slice,
-- @skip@ is the number of indices skipped by the transition,
-- and @lenr@ is the length of the right slice.
-- Each tuple index usually points to several items, but items with the same content
-- are grouped and their values are added in the respective semiring.
newtype TChart e a v = TChart (HM.HashMap (Int,Int,Int,Int) (TCell e a v))

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
  merge chart item@((Transition from _ to) := v) = HM.alter
    insert
    (at, lenl, skip, lenr)
    chart
   where
    insert Nothing     = Just $ tcNew item
    insert (Just cell) = Just $ tcInsert cell item
    at   = sFirst from
    skip = sFirst to - sLast from - 1
    lenl = sliceLen from
    lenr = sliceLen to

-- | Returns the list of transition items starting at @at@,
-- with skip @k@, left slice length @l@, and right slice length @m@.
tcGet :: TChart e a v -> Int -> Int -> Int -> Int -> [TItem e a v]
tcGet (TChart chart) at lenl skip lenr =
  fmap mkItem $ M.toList $ HM.lookupDefault M.empty (at, lenl, skip, lenr) chart
  where mkItem ((a, _, _), v) = a := v

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

-- | An evaluator for verticalizations.
-- Returns the verticalization of a (middle) transition, if possible.
type VertMiddle e a v = Transition e a -> Maybe (a, v)

type VertLeft e a v = Transition e a -> a -> a -> [Transition e a]

type VertRight e a v = a -> Transition e a -> a -> [Transition e a]

-- | An evaluator for merges.
-- Returns possible merges of a given pair of transitions.
type Merge e a v = Transition e a -> Transition e a -> [(Transition e a, v)]

-- | A combined evaluator for verticalizations, merges, and transition inference.
-- Additionally, contains a function for mapping terminal slices to semiring values.
data Eval e a v = Eval
  { evalVertMiddle  :: VertMiddle e a v
  , evalVertLeft :: VertLeft e a v
  , evalVertRight :: VertRight e a v
  , evalMerge :: Merge e a v
  , evalTermSlice  :: Slice a -> v
  }
