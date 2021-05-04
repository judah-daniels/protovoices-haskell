{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
module Parser
  ( Path(..)
  , Parsable
  , Normal
  , Normal'
  , tcGetByLength
  , vcGetByLength
  , parse
  , parseSize
  , parseSilent
  )
where

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
                                                , isJust
                                                )
import           Control.Monad.Reader
import           Debug.Trace                    ( trace )
import qualified Data.Set                      as Set
import qualified Control.Parallel.Strategies   as P
import           Data.Hashable                  ( Hashable )

-- Basic Types
-- ===========

type Normal x = (Eq x, Ord x, Show x, Hashable x, NFData x)
type Normal' x = (Eq x, Show x, NFData x, R.Semiring x)
type Parsable e a v = (Normal e, Normal a, Normal' v)

-- Slices
---------

data Slice a = Slice
  { sFirst   :: !Int
  , sContent :: !(StartStop a)
  , sLast    :: !Int
  }
  deriving (Eq, Ord, Generic, NFData, Hashable)

instance Show a => Show (Slice a) where
  show (Slice f c l) = show f <> "-" <> show c <> "-" <> show l

-- | Return the length of a slice.
sliceLen :: Slice a -> Int
sliceLen (Slice f _ l) = l - f + 1

-- Transitions
--------------

data Transition e a = Transition
  { tLeftSlice  :: !(Slice a)
  , tContent    :: !e
  , tRightSlice :: !(Slice a)
  , t2nd        :: !Bool
  }
  deriving (Eq, Ord, Generic, NFData, Hashable)

instance (Show a, Show e) => Show (Transition e a) where
  show (Transition l c r s) =
    "<" <> show l <> "," <> show c <> "," <> show r <> ">" <> if s
      then "2"
      else ""

transLen :: Transition e a -> Int
transLen (Transition l _ r _) = sLast r - sFirst l + 1

-- Items
--------

data Item i v = (:=)
  { iItem :: !i
  , iValue :: !(S.Score v Int)
  }
  deriving (Generic, NFData)

instance (Show i, Show v) => Show (Item i v) where
  show (i := v) = show i <> " := " <> show v

-- | A transition item.
type TItem e a v = Item (Transition e a) v

-- Vert Items

data Vert e a v = Vert
  { vId :: !Int
  , vTop :: !(Slice a)
  , vOp :: !v
  , vMiddle :: !(TItem e a v)
  }
  deriving (Generic, NFData)

instance (Show e, Show a, Show v) => Show (Vert e a v) where
  show (Vert id top op m) =
    "Vert "
      <> show id
      <> "\n top: "
      <> show top
      <> "\n op:  "
      <> show op
      <> "\n m:   "
      <> show m

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
  { vcNextId :: !Int
  , vcIDs :: !(HM.HashMap (Slice a, Slice a, Maybe (S.LeftId Int)) Int)
  , vcByLength :: !(IM.IntMap [Vert e a v])
  , vcByLengthLeft :: !(IM.IntMap (Set.Set (Int, Slice a, Slice a)))
  , vcByLeftChild :: !(HM.HashMap (Slice a, Int) (Set.Set (Int, Slice a)))
  , vcByRightChild :: !(HM.HashMap (Slice a) [Vert e a v])
  }
  deriving (Generic, NFData)

instance (Show e, Show a, Show v) => Show (VChart e a v) where
  show (VChart n _ is _ _ _) = "VChart (next id: " <> show n <> ")" <> levels
   where
    levels = concatMap showLevel $ IM.toAscList is
    showLevel (l, items) = "\nlevel " <> show l <> ":" <> sitems
      where sitems = concatMap (("\n  " <>) . show) items

vcEmpty :: VChart e a v
vcEmpty = VChart 0 HM.empty IM.empty IM.empty HM.empty HM.empty

vcInsert
  :: (Hashable a, Ord a) => VChart e a v -> (a, v, TItem e a v) -> VChart e a v
vcInsert (VChart nextid ids bylen bylenleft byleft byright) (topContent, op, mid@(tmid := vmid))
  = let left                = tLeftSlice tmid
        right               = tRightSlice tmid
        top = Slice (sFirst left) (Inner topContent) (sLast right)
        idKey               = (top, left, S.leftSide vmid)
        (nextid', ids', id) = case HM.lookup idKey ids of
          Just id -> (nextid, ids, id)
          Nothing -> (nextid + 1, HM.insert idKey nextid ids, nextid)
        vert       = [Vert id top op mid]
        vert'      = Set.singleton (id, top, tLeftSlice tmid)
        vertLeft   = Set.singleton (id, top)
        bylen'     = IM.insertWith (<>) (transLen tmid) vert bylen
        bylenleft' = IM.insertWith (<>) (transLen tmid) vert' bylenleft
        byleft'    = HM.insertWith (<>) (left, transLen tmid) vertLeft byleft
        byright'   = HM.insertWith (<>) right vert byright
    in  VChart nextid' ids' bylen' bylenleft' byleft' byright'

vcMerge
  :: (Foldable t, Ord a, Hashable a)
  => VChart e a v
  -> t (a, v, TItem e a v)
  -> VChart e a v
vcMerge = foldl' vcInsert

vcGetByLength :: VChart e a v -> Int -> [Vert e a v]
vcGetByLength chart len = fromMaybe [] $ IM.lookup len $ vcByLength chart

vcGetByLengthLeft :: VChart e a v -> Int -> [(Int, Slice a, Slice a)]
vcGetByLengthLeft chart len =
  maybe [] Set.toList $ IM.lookup len (vcByLengthLeft chart)

vcGetByLeftChild
  :: (Ord a, Hashable a) => Int -> VChart e a v -> Slice a -> [(Int, Slice a)]
vcGetByLeftChild n chart left =
  Set.toList $ Set.unions $ catMaybes $ getN <$> [2 .. n]
 where
  lefts = vcByLeftChild chart
  getN n = HM.lookup (left, n) lefts

vcGetByRightChild
  :: (Ord a, Hashable a) => Int -> VChart e a v -> Slice a -> [Vert e a v]
vcGetByRightChild n chart right =
  filter notTooLong $ fromMaybe [] $ HM.lookup right $ vcByRightChild chart
  where notTooLong (Vert _ _ _ m) = transLen (iItem m) <= n

-- transition chart
-------------------

-- ops:
-- - get all of length n
-- - get all with left slice l
-- - get all with right slice r

type TCell e a v
  = HM.HashMap
      (Transition e a, Maybe (S.LeftId Int), Maybe (S.RightId Int))
      (S.Score v Int)

data TChart e a v = TChart
  { tcByLength :: !(IM.IntMap (TCell e a v))
  , tcByLeft   :: !(HM.HashMap (Slice a) (TCell e a v))
  , tcByRight  :: !(HM.HashMap (Slice a) (TCell e a v))
  }
  deriving (Show, Generic, NFData)

tcEmpty :: TChart e a v
tcEmpty = TChart IM.empty HM.empty HM.empty

tracePlus k t s1 s2
  | traceLevel >= 1 && (S.score s1 == S.score s2) && isJust (S.score s1) = trace
    (  "adding two "
    <> show (S.score s1)
    <> " for transition "
    <> show t
    <> " while inserting at "
    <> show k
    )
    res
  | otherwise = res
  where res = S.plus s1 s2

-- TODO: there might be room for improvement here
tcInsert :: (Parsable e a v) => TChart e a v -> TItem e a v -> TChart e a v
tcInsert (TChart len left right) item@(t := v) =
  let new    = HM.singleton (t, S.leftSide v, S.rightSide v) v
      len'   = IM.insertWith insert (transLen t) new len
      left'  = HM.insertWith insert (tLeftSlice t) new left
      right' = HM.insertWith insert (tRightSlice t) new right
      result = TChart len' left' right'
  in  if traceLevel >= 4
        then trace ("inserting at " <> show t <> ": " <> S.showScore v) result
        else result
 where
  insert = HM.unionWithKey
    (\k s1 s2 ->
      fromMaybe
          (  error
          $  "Panic! Incompatible score types at "
          <> show t
          <> ": "
          <> show s1
          <> " and "
          <> show s2
          )
        $ S.plus s1 s2 -- (tracePlus k t s1 s2)
    )

tcMerge
  :: (Foldable t, Parsable e a v)
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
  fmap mkItem $ HM.toList $ getter HM.empty key $ field chart
  where mkItem ((t, _, _), v) = t := v

tcGetByLength :: TChart e a v -> Int -> [TItem e a v]
tcGetByLength = tcGetAny tcByLength IM.findWithDefault

tcGetByLeft :: (Ord a, Hashable a) => TChart e a v -> Slice a -> [TItem e a v]
tcGetByLeft = tcGetAny tcByLeft HM.findWithDefault

tcGetByRight :: (Ord a, Hashable a) => TChart e a v -> Slice a -> [TItem e a v]
tcGetByRight = tcGetAny tcByRight HM.findWithDefault

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
  :: VertLeft e a   -- ^ the VertLeft evaluator
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
  :: (R.Semiring v, NFData a, NFData e, NFData v)
  => VertRight e a   -- ^ the VertRight evaluator
  -> Vert e a v      -- ^ the center 'Vert'
  -> TItem e a v     -- ^ the right child transition
  -> [TItem e a v]   -- ^ all possible right parent transitions
vertRight vertr (Vert id top op (tm := vm)) ((Transition rl rt rr _) := vr) =
  fromMaybe [] $ do
    v'   <- S.vertScoresRight id op vm vr
    ir   <- getInner $ sContent rl
    itop <- getInner $ sContent top
    pure $ force $ mkParent v' <$> vertr (ir, rt) ir
  where mkParent v t = Transition top t rr True := v

-- | Infers the possible parent transitions of a split.
merge
  :: (R.Semiring v, NFData a, NFData e, NFData v)
  => Merge e a v   -- ^ the Merge evaluator
  -> TItem e a v   -- ^ the left child transition
  -> TItem e a v   -- ^ the right child transition
  -> [TItem e a v] -- ^ all possible parent transitions
merge mg ((Transition ll lt lr l2nd) := vl) ((Transition !rl !rt !rr _) := vr)
  = case getInner $ sContent lr of
    Just m ->
      force
        $   catMaybes
        $   mkItem
        <$> mg (sContent ll) lt m rt (sContent rr) splitType
    Nothing -> []
 where
  splitType | l2nd                 = RightOfTwo
            | isStop (sContent rr) = LeftOnly
            | otherwise            = LeftOfTwo
  mkItem (!top, !op) =
    (Transition ll top rr l2nd :=) <$> S.mergeScores op vl vr

-- the parsing main loop
------------------------

pmap :: NFData b => (a -> b) -> [a] -> [b]
pmap f = P.withStrategy (P.parList P.rdeepseq) . map f
--pmap = map


pforceList :: NFData a => [a] -> [a]
pforceList = P.withStrategy (P.parList P.rdeepseq)
--pforceList = id

type ParseState e a v = (TChart e a v, VChart e a v)
type ParseOp m e a v = Int -> ParseState e a v -> m (ParseState e a v)

parseStep
  :: (Parsable e a v)
  => (TChart e a v -> VChart e a v -> Int -> IO ())
  -> Eval e e' a a' v
  -> ParseOp IO e a v
parseStep log (Eval eMid eLeft eRight eMerge _ _) n charts = do
  uncurry log charts n
  vertAllMiddles eMid n charts
    >>= vertAllLefts eLeft n
    >>= vertAllRights eRight n
    >>= mergeAll eMerge n

traceVert vert m = if traceLevel >= 1
  then trace ("verting " <> showTrans m <> ": " <> show (isJust res)) res
  else res
 where
  res = vert m
  showSlice (Slice a _ b) = "(" <> show a <> "," <> show b <> ")"
  showTrans ((Transition l _ r _) := v) = showSlice l <> "-" <> showSlice r

-- | Verticalizes all edges of length @n@.
vertAllMiddles
  :: (Monad m, Parsable e a v) => VertMiddle e a v -> ParseOp m e a v
vertAllMiddles evalMid n (!tchart, !vchart) = do
  let ts        = tcGetByLength tchart n
      !newVerts = catMaybes $ pmap (vertMiddle evalMid) $!! ts
      vchart'   = vcMerge vchart newVerts
  return (tchart, vchart')

-- | Perform all left verts where either @l@ or @m@ have length @n@
vertAllLefts :: (Monad m, Parsable e a v) => VertLeft e a -> ParseOp m e a v
vertAllLefts evalLeft n (!tchart, !vchart) = do
  let -- left = n (and middle <= n)
      leftn = pmap (uncurry $ vertLeft evalLeft) $!! do -- in list monad
        left      <- tcGetByLength tchart n
        (id, top) <- vcGetByLeftChild n vchart (tRightSlice $ iItem left)
        pure (left, (top, id))

      -- middle = n (and left < n)
      vertn = pmap (uncurry $ vertLeft evalLeft) $!! do -- in list monad
        (id, top, lslice) <- vcGetByLengthLeft vchart n
        left              <- filter (\i -> transLen (iItem i) < n)
          $ tcGetByRight tchart lslice
        pure (left, (top, id))

      -- insert new transitions into chart
      tchart' = foldl' tcMerge (foldl' tcMerge tchart leftn) vertn
  return (tchart', vchart)

-- | Perform all right verts where either @r@ or @m@ have length @n@
vertAllRights :: (Monad m, Parsable e a v) => VertRight e a -> ParseOp m e a v
vertAllRights evalRight n (!tchart, !vchart) = do
  let -- right = n (and middle <= n)
      !rightn = force $ pmap (uncurry $ vertRight evalRight) $!! do -- in list monad
        right <- tcGetByLength tchart n
        vert  <- vcGetByRightChild n vchart (tLeftSlice $ iItem right)
        pure (vert, right)
        --traceVertRight vert right $ vertRight evalRight vert right

      -- middle = n (and left < n)
      !vertn = force $ pmap (uncurry $ vertRight evalRight) $!! do -- in list monad
        vert  <- vcGetByLength vchart n
        right <- filter (\i -> transLen (iItem i) < n)
          $ tcGetByLeft tchart (tRightSlice $ iItem $ vMiddle vert)
        pure (vert, right)
        -- traceVertRight vert right $ vertRight evalRight vert right

      -- insert new transitions into chart
      !tchart' = foldl' tcMerge (foldl' tcMerge tchart rightn) vertn
  return (tchart', vchart)

-- | perform all merges where either @l@ or @r@ have length @n@
mergeAll
  :: forall e a v m
   . (Monad m, Parsable e a v)
  => Merge e a v
  -> ParseOp m e a v
mergeAll mrg n (!tchart, !vchart) = do
  let !byLen = force $ tcGetByLength tchart n

      -- left = n (and right <= n)
      !leftn = pmap (uncurry (merge mrg)) $!! do
        left  <- byLen
        right <- filter (\r -> transLen (iItem r) <= n)
          $ tcGetByLeft tchart (tRightSlice $ iItem left)
        pure (left, right)
        -- traceMerge ("left = " <> show n) left right res

      -- right = n (and left < n)
      !rightn = pmap (uncurry (merge mrg)) $!! do
        right <- byLen
        left  <- filter (\l -> transLen (iItem l) < n)
          $ tcGetByRight tchart (tLeftSlice $ iItem right)
        pure (left, right)
        -- traceMerge ("right = " <> show n) left right (left, right)

      -- insert new transitions into chart
      !tchart' = foldl' tcMerge (foldl' tcMerge tchart leftn) rightn
  return (tchart', vchart)

-- parsing entry point
----------------------

data Path a e = Path !a !e !(Path a e)
              | PathEnd !a

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
mapEdges f (Path l m tail) = f l m r : mapEdges f tail where r = pathHead tail
mapEdges f (PathEnd _    ) = []

-- | The main entrypoint to the parser.
-- Expects an evaluator for the specific grammar
-- and an input path.
-- Returns the combined semiring value of all full derivations.
parse
  :: Parsable e a v
  => (TChart e a v -> VChart e a v -> Int -> IO ())
  -> Eval e e' a a' v
  -> Path a' e'
  -> IO v
parse log eval path = do
  (tfinal, _) <- foldM (flip $ parseStep log eval)
                       (tinit, vcEmpty)
                       [2 .. len - 1]
  let goals = tcGetByLength tfinal len
  return $ R.sum $ catMaybes $ S.score . iValue <$> goals
 where
  wrapPath (Path a e rst) = Path (Inner a) (Just e) $ wrapPath rst
  wrapPath (PathEnd a   ) = Path (Inner a) Nothing $ PathEnd (:⋉)
  path' = Path (:⋊) Nothing $ wrapPath path
  len   = pathLen path'
  slicePath =
    mapNodesWithIndex 0 (\i n -> Slice i (evalSlice eval <$> n) i) path'
  mkTrans l e r = mk
    <$> evalThaw eval (sContent l) e (sContent r) (isStop $ sContent r)
    where mk (e, v) = Transition l e r False := S.SVal v
  trans0 = mapEdges mkTrans slicePath
  tinit  = tcMerge tcEmpty $ concat trans0

logSize tc vc n = do
  putStrLn $ "parsing level " <> show n
  putStrLn $ "transitions: " <> show (length $ tcGetByLength tc n)
  putStrLn $ "verts: " <> show (length $ vcGetByLength vc (n - 1))

parseSize :: Parsable e a v => Eval e e' a a' v -> Path a' e' -> IO v
parseSize = parse logSize

logNone tc vc n = pure ()

parseSilent :: Parsable e a v => Eval e e' a a' v -> Path a' e' -> IO v
parseSilent = parse logNone
