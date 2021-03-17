{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
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
import           Data.Maybe                     ( fromMaybe
                                                , catMaybes
                                                )

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
  , sContent :: StartStop a
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
  , vTop :: Slice a
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
  { vcIDs :: M.Map (StartStop a, Slice a, Maybe Int) Int
  , vcLeftMap :: M.Map (Slice a, Maybe Int) [(Slice a, Int)]
  , vcRightMap :: M.Map (Maybe Int, Slice a) [Vert e a v]
  }

vcInsert
  :: Ord a
  => Maybe (VCell e a v)
  -> (StartStop a, v, TItem e a v)
  -> WithID (VCell e a v)
vcInsert cell (top, op, mid@(tmid := vmid)) = do
  (i, ids') <- lookupId keyIDs ids
  let left'  = M.insertWith (<>) keyLeft [(topSlice, i)] leftMap
      right' = M.insertWith (<>) keyRight [Vert i topSlice op mid] rightMap
  pure $ VCell ids' left' right'
 where
  (ids, leftMap, rightMap) = case cell of
    Just (VCell is lm rm) -> (is, lm, rm)
    Nothing               -> (mempty, mempty, mempty)
  keyIDs   = (top, tLeftSlice tmid, S.leftSide vmid)
  keyLeft  = (tLeftSlice tmid, S.leftSide vmid)
  keyRight = (S.rightSide vmid, tRightSlice tmid)
  topSlice = Slice (sFirst $ tLeftSlice tmid) top (sLast $ tRightSlice tmid)

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
  => t (StartStop a, v, TItem e a v)
  -> VChart e a v
vcFromFoldable = vcMerge vcEmpty

-- | Merges a collection of slice items into a slice chart,
-- adding the values of duplicate items.
vcMerge
  :: (R.Semiring v, Foldable t, Eq a, Ord a)
  => VChart e a v
  -> t (StartStop a, v, TItem e a v)
  -> VChart e a v
vcMerge (VChart chart nextid) ss = VChart chart' nextid'
 where
  (chart', nextid') = runState (foldlM merge chart ss) nextid
  merge chart new@(_, _, mid@(tmid := _)) = do
    cell' <- vcInsert (HM.lookup key chart) new
    pure $ HM.insert key cell' chart
   where
    at  = sFirst $ tLeftSlice tmid
    n   = transLen tmid
    key = (at, n)

-- | Returns the list of slice items starting at @at@ with length @len@.
vcGetFromLeft
  :: Ord a
  => VChart e a v
  -> Int
  -> Int
  -> Slice a
  -> Maybe Int
  -> [(Slice a, Int)]
vcGetFromLeft (VChart chart _) at len lSlice lSide =
  case HM.lookup (at, len) chart of
    Nothing                  -> []
    Just (VCell _ leftMap _) -> M.findWithDefault [] (lSlice, lSide) leftMap

-- | The number of items in layer @n@ (i.e. with length @n@).
vcItemsInLayer :: VChart e a v -> Int -> Int
vcItemsInLayer (VChart chart _) n =
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
-- TODO: factor out some of the StartStop stuff when Inner should be guaranteed

-- | An evaluator for verticalizations.
-- Returns the verticalization of a (middle) transition, if possible.
type VertMiddle e a v = (StartStop a, e, StartStop a) -> Maybe (a, v)

-- | An evaluator returning the possible left parent edges of a verticalization.
type VertLeft e a v = (e, StartStop a) -> StartStop a -> [e]

-- | An evaluator returning the possible right parent edges of a verticalization.
type VertRight e a v = (StartStop a, e) -> StartStop a -> [e]

-- | An evaluator for merges.
-- Returns possible merges of a given pair of transitions.
type Merge e a v = e -> StartStop a -> e -> [(e, v)]

-- | A combined evaluator for verticalizations, merges, and thaws.
-- Additionally, contains a function for mapping terminal slices to semiring values.
data Eval e e' a v = Eval
  { evalVertMiddle  :: VertMiddle e a v
  , evalVertLeft :: VertLeft e a v
  , evalVertRight :: VertRight e a v
  , evalMerge :: Merge e a v
  , evalThaw  :: StartStop a -> e' -> StartStop a -> [(e, v)]
  }

-- applying evaluators
----------------------

-- | Verticalizes the two slices of a (middle) transition, if possible.
vertMiddle
  :: VertMiddle e a v          -- ^ the VertMiddle evaluator
  -> TItem e a v               -- ^ the middle transition
  -> Maybe (a, v, TItem e a v) -- ^ the top slice, vert operation, and middle transition
vertMiddle vertm im@((Transition l m r) := vm) = do
  (top, op) <- vertm (sContent l, m, sContent r)
  pure (top, op, im)

-- | Infers the possible left parent transitions of a verticalization.
vertLeft
  :: VertLeft e a v -- ^ the VertLeft evaluator
  -> TItem e a v    -- ^ the left child transition
  -> (Slice a, Int) -- ^ the Vert's top slice and ID
  -> [TItem e a v]  -- ^ all possible left parent transitions
vertLeft vertl ((Transition ll lt lr) := vleft) (top, newId) =
  case S.vertScoresLeft newId vleft of
    Just v' -> mkParent v' <$> vertl (lt, sContent lr) (sContent top)
    Nothing -> []
  where mkParent v t = Transition ll t top := v

-- | Infers the possible right parent transitions of a verticalization.
vertRight
  :: R.Semiring v
  => VertRight e a v -- ^ the VertRight evaluator
  -> Vert e a v      -- ^ the center 'Vert'
  -> TItem e a v     -- ^ the right child transition
  -> [TItem e a v]   -- ^ all possible right parent transitions
vertRight vertr (Vert id top op (tm := vm)) ((Transition rl rt rr) := vr) =
  case S.vertScoresRight id op vm vr of
    Just v' -> mkParent v' <$> vertr (sContent rl, rt) (sContent top)
    Nothing -> []
  where mkParent v t = Transition top t rr := v

-- | Infers the possible parent transitions of a split.
merge
  :: R.Semiring v
  => Merge e a v   -- ^ the Merge evaluator
  -> TItem e a v   -- ^ the left child transition
  -> TItem e a v   -- ^ the right child transition
  -> [TItem e a v] -- ^ all possible parent transitions
merge mg ((Transition ll lt lr) := vl) ((Transition rl rt rr) := vr) =
  catMaybes $ mkItem <$> mg lt (sContent lr) rt
  where mkItem (top, op) = (Transition ll top rr :=) <$> S.mergeScores op vl vr

-- the parsing main loop
------------------------

type ParseOp e a v = State (TChart e a v, VChart e a v) ()

vertAllMiddles :: Int -> ParseOp e a v
vertAllMiddles n = undefined

vertAllLefts :: Int -> ParseOp e a v
vertAllLefts n = undefined

vertAllRights :: Int -> ParseOp e a v
vertAllRights n = undefined

mergeAll :: Int -> ParseOp e a v
mergeAll n = undefined

parseStep :: Eval e e' a v -> Int -> ParseOp e a v
parseStep eval n = do
  vertAllMiddles n
  vertAllLefts n
  vertAllRights n
  mergeAll n

-- parsing entry point
----------------------

data Path e a = Path a e (Path e a)
              | PathEnd a

pathLen :: Path e a -> Int
pathLen (Path _ _ tail) = pathLen tail + 1
pathLen (PathEnd _    ) = 1

pathHead :: Path e a -> a
pathHead (Path l _ _) = l
pathHead (PathEnd l ) = l

mapNodesWithIndex :: Int -> (Int -> a -> b) -> Path e a -> Path e b
mapNodesWithIndex i f (Path l m tail) =
  Path (f i l) m (mapNodesWithIndex (i + 1) f tail)
mapNodesWithIndex i f (PathEnd n) = PathEnd (f i n)

mapEdges :: (a -> e -> a -> b) -> Path e a -> [b]
mapEdges f (Path l m tail) = f l m (pathHead tail) : mapEdges f tail
mapEdges f (PathEnd _    ) = []

parse
  :: (R.Semiring v, Ord e, Ord a) => Eval e e' a v -> Path e' (StartStop a) -> v
parse eval path = R.sum $ catMaybes $ S.score . iValue <$> goals
 where
  len       = pathLen path
  slicePath = mapNodesWithIndex 0 (\i n -> Slice i n i) path
  mkTrans l e r = mk <$> evalThaw eval (sContent l) e (sContent r)
    where mk (e, v) = Transition l e r := S.SVal v
  trans0 = mapEdges mkTrans slicePath
  tinit  = tcFromFoldable $ concat trans0
  (_, (tfinal, _)) =
    runState (mapM_ (parseStep eval) [2 .. len - 1]) (tinit, vcEmpty)
  goals = tcGoals tfinal len
