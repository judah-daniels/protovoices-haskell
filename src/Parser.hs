{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Parser where

import           Common
import qualified Scoring                       as S

import qualified Data.Map.Strict               as M
import qualified Data.HashMap.Strict           as HM
import qualified Data.IntMap.Strict            as IM
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
import           Control.Monad.Reader

-- Basic Types
-- ===========

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
  , t2nd        :: Bool
  }
  deriving (Eq, Ord, Generic)

transLen :: Transition e a -> Int
transLen (Transition l _ r _) = sLast r - sFirst l + 1

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

-- vert chart
-------------

-- ops:
-- - get all of len n
-- - get all with left child = x
-- - get all with right child = x
-- - check ID for (top,left,leftid)

data VChart e a v = VChart
  { vcNextId :: Int
  , vcIDs :: M.Map (Slice a, Slice a, Maybe Int) Int
  , vcByLength :: IM.IntMap [Vert e a v]
  , vcByLeftChild :: M.Map (Slice a) [Vert e a v]
  , vcByRightChild :: M.Map (Slice a) [Vert e a v]
  }

vcEmpty :: VChart e a v
vcEmpty = VChart 0 M.empty IM.empty M.empty M.empty

vcInsert :: Ord a => VChart e a v -> (a, v, TItem e a v) -> VChart e a v
vcInsert (VChart nextid ids bylen byleft byright) (topContent, op, mid@(tmid := vmid))
  = let left                = tLeftSlice tmid
        right               = tRightSlice tmid
        top = Slice (sFirst left) (Inner topContent) (sLast right)
        idKey               = (top, left, S.leftSide vmid)
        (nextid', ids', id) = case M.lookup idKey ids of
          Just id -> (nextid, ids, id)
          Nothing -> (nextid + 1, M.insert idKey nextid ids, nextid)
        vert     = [Vert id top op mid]
        bylen'   = IM.insertWith (<>) (transLen tmid) vert bylen
        byleft'  = M.insertWith (<>) left vert byleft
        byright' = M.insertWith (<>) right vert byright
    in  VChart nextid' ids' bylen' byleft' byright'

vcMerge
  :: (Foldable t, Ord a)
  => VChart e a v
  -> t (a, v, TItem e a v)
  -> VChart e a v
vcMerge = foldl' vcInsert

vcGetByLength :: (VChart e a v -> Int -> [Vert e a v])
vcGetByLength chart len = fromMaybe [] $ IM.lookup len $ vcByLength chart

vcGetByLeftChild :: (Ord a0 => VChart e a0 v -> Slice a0 -> [Vert e a0 v])
vcGetByLeftChild chart left =
  fromMaybe [] $ M.lookup left $ vcByLeftChild chart

vcGetByRightChild :: (Ord a0 => VChart e a0 v -> Slice a0 -> [Vert e a0 v])
vcGetByRightChild chart right =
  fromMaybe [] $ M.lookup right $ vcByRightChild chart

-- transition chart
-------------------

-- ops:
-- - get all of length n
-- - get all with left slice l
-- - get all with right slice r

type TCell e a v = M.Map (Transition e a, Maybe Int, Maybe Int) (S.Score v Int)

data TChart e a v = TChart
  { tcByLength :: IM.IntMap (TCell e a v)
  , tcByLeft   :: M.Map (Slice a) (TCell e a v)
  , tcByRight  :: M.Map (Slice a) (TCell e a v)
  }

tcEmpty :: TChart e a v
tcEmpty = TChart IM.empty M.empty M.empty

-- TODO: there might be room for improvement here
tcInsert
  :: (Ord a, R.Semiring v, Ord e) => TChart e a v -> TItem e a v -> TChart e a v
tcInsert (TChart len left right) item@(t := v) =
  let new    = M.singleton (t, S.leftSide v, S.rightSide v) v
      len'   = IM.insertWith insert (transLen t) new len
      left'  = M.insertWith insert (tLeftSlice t) new left
      right' = M.insertWith insert (tRightSlice t) new right
  in  TChart len' left' right'
  where insert = M.unionWith (\s1 s2 -> fromMaybe s2 (S.plus s1 s2))

tcMerge
  :: (R.Semiring v, Foldable t, Ord a, Ord e)
  => TChart e a v
  -> t (TItem e a v)
  -> TChart e a v
tcMerge = foldl' tcInsert

tcGetAny
  :: (TChart e a v -> m)
  -> (TCell e a v -> k -> m -> TCell e a v)
  -> TChart e a v
  -> k
  -> [TItem e a v]
tcGetAny field getter chart key =
  fmap mkItem $ M.toList $ getter M.empty key $ field chart
  where mkItem ((t, _, _), v) = t := v

tcGetByLength :: TChart e a v -> Int -> [TItem e a v]
tcGetByLength = tcGetAny tcByLength IM.findWithDefault

tcGetByLeft :: Ord a => TChart e a v -> Slice a -> [TItem e a v]
tcGetByLeft = tcGetAny tcByLeft M.findWithDefault

tcGetByRight :: Ord a => TChart e a v -> Slice a -> [TItem e a v]
tcGetByRight = tcGetAny tcByRight M.findWithDefault

-- parsing machinery
-- =================

-- applying evaluators
----------------------
-- TODO: add checks that adjacent transitions and slices match?

-- | Verticalizes the two slices of a (middle) transition, if possible.
vertMiddle
  :: VertMiddle e a v                    -- ^ the VertMiddle evaluator
  -> TItem e a v                         -- ^ the middle transition
  -> Maybe (a, v, TItem e a v) -- ^ the top slice, vert operation,
                                         -- and middle transition
vertMiddle vertm im@((Transition l m r _) := vm) = do
  il        <- getInner $ sContent l
  ir        <- getInner $ sContent r
  (top, op) <- vertm (il, m, ir)
  pure (top, op, im)

-- | Infers the possible left parent transitions of a verticalization.
vertLeft
  :: VertLeft e a v -- ^ the VertLeft evaluator
  -> TItem e a v    -- ^ the left child transition
  -> (Slice a, Int) -- ^ the Vert's top slice and ID
  -> [TItem e a v]  -- ^ all possible left parent transitions
vertLeft vertl ((Transition ll lt lr is2nd) := vleft) (top, newId)
  | is2nd = []
  | otherwise = fromMaybe [] $ do
    v'   <- S.vertScoresLeft newId vleft
    ir   <- getInner $ sContent lr
    itop <- getInner $ sContent top
    pure $ mkParent v' <$> vertl (lt, ir) itop
  where mkParent v t = Transition ll t top False := v

-- | Infers the possible right parent transitions of a verticalization.
vertRight
  :: R.Semiring v
  => VertRight e a v -- ^ the VertRight evaluator
  -> Vert e a v      -- ^ the center 'Vert'
  -> TItem e a v     -- ^ the right child transition
  -> [TItem e a v]   -- ^ all possible right parent transitions
vertRight vertr (Vert id top op (tm := vm)) ((Transition rl rt rr _) := vr) =
  fromMaybe [] $ do
    v'   <- S.vertScoresRight id op vm vr
    ir   <- getInner $ sContent rl
    itop <- getInner $ sContent top
    pure $ mkParent v' <$> vertr (ir, rt) ir
  where mkParent v t = Transition top t rr True := v

-- | Infers the possible parent transitions of a split.
merge
  :: R.Semiring v
  => Merge e a v   -- ^ the Merge evaluator
  -> TItem e a v   -- ^ the left child transition
  -> TItem e a v   -- ^ the right child transition
  -> [TItem e a v] -- ^ all possible parent transitions
merge mg ((Transition ll lt lr l2nd) := vl) ((Transition rl rt rr _) := vr) =
  case getInner $ sContent lr of
    Just m ->
      catMaybes $ mkItem <$> mg (sContent ll) lt m rt (sContent rr) l2nd
    Nothing -> []
 where
  mkItem (top, op) = (Transition ll top rr l2nd :=) <$> S.mergeScores op vl vr

-- the parsing main loop
------------------------

type ParseState e a v = (TChart e a v, VChart e a v)
type ParseOp m e a v = Int -> ParseState e a v -> m (ParseState e a v)

parseStep :: (R.Semiring v, Ord a, Ord e) => Eval e e' a v -> ParseOp IO e a v
parseStep (Eval eMid eLeft eRight eMerge _) n charts = do
  putStrLn $ "parsing level " <> show n
  vertAllMiddles eMid n charts
    >>= vertAllLefts eLeft n
    >>= vertAllRights eRight n
    >>= mergeAll eMerge n

-- | Verticalizes all edges of length @n@.
vertAllMiddles
  :: (R.Semiring v, Ord a, Monad m) => VertMiddle e a v -> ParseOp m e a v
vertAllMiddles evalMid n (tchart, vchart) = do
  let ts       = tcGetByLength tchart n
      newVerts = catMaybes $ vertMiddle evalMid <$> ts
      vchart'  = vcMerge vchart newVerts
  return (tchart, vchart')

-- | Perform all left verts where either @l@ or @m@ have length @n@
vertAllLefts
  :: (R.Semiring v, Ord a, Ord e, Monad m) => VertLeft e a v -> ParseOp m e a v
vertAllLefts evalLeft n (tchart, vchart) = do
  let -- left = n (and middle <= n)
      leftn = do -- in list monad
        left            <- tcGetByLength tchart n
        Vert id top _ _ <- vcGetByLeftChild vchart (tRightSlice $ iItem left)
        vertLeft evalLeft left (top, id)

      -- middle = n (and left < n)
      vertn = do -- in list monad
        (Vert id top _ mid) <- vcGetByLength vchart n
        left                <- filter (\i -> transLen (iItem i) < n)
          $ tcGetByRight tchart (tLeftSlice $ iItem mid)
        vertLeft evalLeft left (top, id)

      -- insert new transitions into chart
      tchart' = tcMerge (tcMerge tchart leftn) vertn
  return (tchart', vchart)

-- | Perform all right verts where either @r@ or @m@ have length @n@
vertAllRights
  :: (R.Semiring v, Ord a, Ord e, Monad m) => VertRight e a v -> ParseOp m e a v
vertAllRights evalRight n (tchart, vchart) = do
  let -- right = n (and middle <= n)
      rightn = do -- in list monad
        right <- tcGetByLength tchart n
        vert  <- vcGetByRightChild vchart (tLeftSlice $ iItem right)
        vertRight evalRight vert right

      -- middle = n (and left < n)
      vertn = do -- in list monad
        vert  <- vcGetByLength vchart n
        right <- filter (\i -> transLen (iItem i) < n)
          $ tcGetByLeft tchart (tRightSlice $ iItem $ vMiddle vert)
        vertRight evalRight vert right

      -- insert new transitions into chart
      tchart' = tcMerge (tcMerge tchart rightn) vertn
  return (tchart', vchart)

-- | perform all merges where either @l@ or @r@ have length @n@
mergeAll
  :: (R.Semiring v, Ord a, Ord e, Monad m) => Merge e a v -> ParseOp m e a v
mergeAll mrg n (tchart, vchart) = do
  let -- left = n (and right <= n)
      leftn = do
        left  <- tcGetByLength tchart n
        right <- tcGetByLeft tchart (tRightSlice $ iItem left)
        merge mrg left right

      -- right = n (and left <= n)
      rightn = do
        right <- tcGetByLength tchart n
        left  <- filter (\l -> transLen (iItem l) < n)
          $ tcGetByRight tchart (tLeftSlice $ iItem right)
        merge mrg left right

      -- insert new transitions into chart
      tchart' = tcMerge (tcMerge tchart leftn) rightn
  return (tchart', vchart)

-- parsing entry point
----------------------

data Path a e = Path a e (Path a e)
              | PathEnd a

instance (Show a, Show e) => Show (Path a e) where
  show (Path a e rst) = show a <> "\n+-" <> show e <> "\n" <> show rst
  show (PathEnd a   ) = show a

pathLen :: Path a e -> Int
pathLen (Path _ _ tail) = pathLen tail + 1
pathLen (PathEnd _    ) = 1

pathHead :: Path a e -> a
pathHead (Path l _ _) = l
pathHead (PathEnd l ) = l

mapNodesWithIndex :: Int -> (Int -> a -> b) -> Path a e -> Path b e
mapNodesWithIndex i f (Path l m tail) =
  Path (f i l) m (mapNodesWithIndex (i + 1) f tail)
mapNodesWithIndex i f (PathEnd n) = PathEnd (f i n)

mapEdges :: (a -> e -> a -> b) -> Path a e -> [b]
mapEdges f (Path l m tail) = f l m (pathHead tail) : mapEdges f tail
mapEdges f (PathEnd _    ) = []

-- | The main entrypoint to the parser.
-- Expects an evaluator for the specific grammar
-- and an input path.
-- Returns the combined semiring value of all full derivations.
parse
  :: (R.Semiring v, Ord e, Ord a)
  => Eval e e' a v
  -> Path (StartStop a) e'
  -> IO v
parse eval path = do
  (tfinal, _) <- foldM (flip $ parseStep eval) (tinit, vcEmpty) [2 .. len - 1]
  let goals = tcGetByLength tfinal len
  return $ R.sum $ catMaybes $ S.score . iValue <$> goals
 where
  len       = pathLen path
  slicePath = mapNodesWithIndex 0 (\i n -> Slice i n i) path
  mkTrans l e r = mk <$> evalThaw eval (sContent l) e (sContent r)
    where mk (e, v) = Transition l e r False := S.SVal v
  trans0 = mapEdges mkTrans slicePath
  tinit  = tcMerge tcEmpty $ concat trans0
