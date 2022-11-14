{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Parser
  ( Parsable
  , Normal
  , Normal'
  , tcGetByLength
  , vcGetByLength
  , parse
  , parseSize
  , parseSilent
  , logTikz
  ) where

import           Common
import qualified Scoring.FunTyped              as S

import qualified Data.HashMap.Strict           as HM
import qualified Data.IntMap.Strict            as IM
import qualified Data.Semiring                 as R

import           Control.Monad.State           as ST

import           Control.DeepSeq
import qualified Control.Parallel.Strategies   as P
import           Data.Foldable                  ( foldl' )
import           Data.Hashable                  ( Hashable
                                                , hash
                                                , hashWithSalt
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import qualified Data.Set                      as Set
import           GHC.Generics                   ( Generic )

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
  , sID      :: !Int
  , sLast    :: !Int
  }
  deriving (Eq, Ord, Generic, NFData)

instance Hashable (Slice a) where
  hashWithSalt s (Slice _ _ i _) = hashWithSalt s i

instance Show a => Show (Slice a) where
  show (Slice f c i l) =
    show f <> "-" <> show c <> "@" <> show i <> "-" <> show l

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
  { iItem  :: !i
  , iValue :: !(S.Score v Int)
  }
  deriving (Generic, NFData)

instance (Show i, Show v) => Show (Item i v) where
  show (i := v) = show i <> " := " <> show v

-- | A transition item.
type TItem e a v = Item (Transition e a) v

-- Vert Items

data Vert e a v = Vert
  { vTop    :: !(Slice a)
  , vOp     :: !v
  , vMiddle :: !(TItem e a v)
  }
  deriving (Generic, NFData)

instance (Show e, Show a, Show v) => Show (Vert e a v) where
  show (Vert top op m) =
    "Vert"
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
  { vcNextId       :: !Int
  , vcIDs          :: !(HM.HashMap (Int, Int) Int)
  , vcByLength     :: !(IM.IntMap [Vert e a v])
  , vcByLengthLeft :: !(IM.IntMap (Set.Set (Int, Slice a, Slice a)))
  , vcByLeftChild  :: !(HM.HashMap (Int, Int) (Set.Set (Int, Slice a)))
  , vcByRightChild :: !(HM.HashMap Int [Vert e a v])
  }
  deriving (Generic, NFData)

instance (Show e, Show a, Show v) => Show (VChart e a v) where
  show (VChart n _ is _ _ _) = "VChart (next id: " <> show n <> ")" <> levels
   where
    levels = concatMap showLevel $ IM.toAscList is
    showLevel (l, items) = "\nlevel " <> show l <> ":" <> sitems
      where sitems = concatMap (("\n  " <>) . show) items

vcEmpty :: Int -> VChart e a v
vcEmpty n = VChart (n + 1) HM.empty IM.empty IM.empty HM.empty HM.empty

vcInsert
  :: (Hashable a, Ord a) => VChart e a v -> (a, v, TItem e a v) -> VChart e a v
vcInsert (VChart nextid ids bylen bylenleft byleft byright) (topContent, op, mid@(tmid := _))
  = let left               = tLeftSlice tmid
        right              = tRightSlice tmid
        idKey              = (sID left, sID right)
        (nextid', ids', i) = case HM.lookup idKey ids of
          Just i' -> (nextid, ids, i')
          Nothing -> (nextid + 1, HM.insert idKey nextid ids, nextid)
        top        = Slice (sFirst left) (Inner topContent) i (sLast right)
        vert       = [Vert top op mid]
        vert'      = Set.singleton (i, top, tLeftSlice tmid)
        vertl      = Set.singleton (i, top)
        bylen'     = IM.insertWith (<>) (transLen tmid) vert bylen
        bylenleft' = IM.insertWith (<>) (transLen tmid) vert' bylenleft
        byleft'    = HM.insertWith (<>) (sID left, transLen tmid) vertl byleft
        byright'   = HM.insertWith (<>) (sID right) vert byright
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
vcGetByLeftChild maxn chart left =
  Set.toList $ Set.unions $ catMaybes $ getN <$> [2 .. maxn]
 where
  lefts = vcByLeftChild chart
  getN n = HM.lookup (sID left, n) lefts

vcGetByRightChild
  :: (Ord a, Hashable a) => Int -> VChart e a v -> Slice a -> [Vert e a v]
vcGetByRightChild n chart right =
  filter notTooLong $ fromMaybe [] $ HM.lookup (sID right) $ vcByRightChild
    chart
  where notTooLong (Vert _ _ m) = transLen (iItem m) <= n

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

-- TODO: there might be room for improvement here
tcInsert :: (Parsable e a v) => TChart e a v -> TItem e a v -> TChart e a v
tcInsert (TChart len left right) (t := v) =
  let new    = HM.singleton (t, S.leftSide v, S.rightSide v) v
      len'   = IM.insertWith insert (transLen t) new len
      left'  = HM.insertWith insert (tLeftSlice t) new left
      right' = HM.insertWith insert (tRightSlice t) new right
  in  TChart len' left' right'
  where insert = HM.unionWithKey (\_ s1 s2 -> S.addScores s1 s2)

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
  :: UnspreadMiddle e a v                    -- ^ the UnspreadMiddle evaluator
  -> TItem e a v                         -- ^ the middle transition
  -> Maybe (a, v, TItem e a v) -- ^ the top slice, vert operation,
                                         -- and middle transition
vertMiddle vertm im@((Transition l m r _) := _) = do
  il        <- getInner $ sContent l
  ir        <- getInner $ sContent r
  (top, op) <- vertm (il, m, ir)
  pure (top, op, im)

-- | Infers the possible left parent transitions of a verticalization.
vertLeft
  :: (Show a, Show e, R.Semiring v, Show v)
  => UnspreadLeft e a   -- ^ the UnspreadLeft evaluator
  -> TItem e a v    -- ^ the left child transition
  -> (Slice a, Int) -- ^ the Vert's top slice and ID
  -> [TItem e a v]  -- ^ all possible left parent transitions
vertLeft vertl (tleft@(Transition ll lt lr is2nd) := vleft) (top, newId)
  | is2nd = []
  | otherwise = fromMaybe err $ do
    ir   <- getInner $ sContent lr
    itop <- getInner $ sContent top
    pure $ mkParent v' <$> vertl (lt, ir) itop
 where
  err = error $ "Illegal left-vert: left=" <> show tleft <> ", vert=" <> show
    (top, newId)
  v' = S.vertScoresLeft newId vleft
  mkParent v t = Transition ll t top False := v

-- | Infers the possible right parent transitions of a verticalization.
vertRight
  :: (R.Semiring v, NFData a, NFData e, NFData v, Show e, Show a, Show v)
  => UnspreadRight e a   -- ^ the UnspreadRight evaluator
  -> Vert e a v      -- ^ the center 'Vert'
  -> TItem e a v     -- ^ the right child transition
  -> [TItem e a v]   -- ^ all possible right parent transitions
vertRight vertr vert@(Vert top op (_ := vm)) tright@((Transition rl rt rr _) := vr)
  = fromMaybe err $ do
    ir <- getInner $ sContent rl
    pure $ force $ mkParent v' <$> vertr (ir, rt) ir
 where
  err =
    error
      $  "Illegal right-vert: vert="
      <> show vert
      <> ", right="
      <> show tright
  v' = S.vertScoresRight (sID top) op vm vr
  mkParent v t = Transition top t rr True := v

-- | Infers the possible parent transitions of a split.
merge
  :: (R.Semiring v, NFData a, NFData e, NFData v, Show v)
  => Unsplit e a v   -- ^ the Unsplit evaluator
  -> TItem e a v   -- ^ the left child transition
  -> TItem e a v   -- ^ the right child transition
  -> [TItem e a v] -- ^ all possible parent transitions
merge mg ((Transition ll lt lr l2nd) := vl) ((Transition _ !rt !rr _) := vr) =
  case getInner $ sContent lr of
    Just m ->
      force $ mkItem <$> mg (sContent ll) lt m rt (sContent rr) splitType
    Nothing -> error "trying to merge at a non-content slice"
 where
  splitType | l2nd                 = RightOfTwo
            | isStop (sContent rr) = SingleOfOne
            | otherwise            = LeftOfTwo
  mkItem (!top, !op) = Transition ll top rr l2nd := S.mergeScores op vl vr

-- the parsing main loop
------------------------

pmap :: NFData b => (a -> b) -> [a] -> [b]
pmap f = P.withStrategy (P.parList P.rdeepseq) . map f
--pmap = map


-- pforceList :: NFData a => [a] -> [a]
-- pforceList = P.withStrategy (P.parList P.rdeepseq)
-- --pforceList = id

type ParseState e a v = (TChart e a v, VChart e a v)
type ParseOp m e a v = Int -> ParseState e a v -> m (ParseState e a v)

parseStep
  :: (Parsable e a v)
  => (TChart e a v -> VChart e a v -> Int -> IO ())
  -> Eval e e' a a' v
  -> ParseOp IO e a v
parseStep logCharts (Eval eMid eLeft eRight eUnsplit _ _) n charts = do
  uncurry logCharts charts n
  vertAllMiddles eMid n charts
    >>= vertAllLefts eLeft n
    >>= vertAllRights eRight n
    >>= mergeAll eUnsplit n

-- | Verticalizes all edges of length @n@.
vertAllMiddles
  :: (Monad m, Parsable e a v) => UnspreadMiddle e a v -> ParseOp m e a v
vertAllMiddles evalMid n (!tchart, !vchart) = do
  let ts        = tcGetByLength tchart n
      !newVerts = catMaybes $ pmap (vertMiddle evalMid) $!! ts
      vchart'   = vcMerge vchart newVerts
  return (tchart, vchart')

-- | Perform all left verts where either @l@ or @m@ have length @n@
vertAllLefts
  :: (Monad m, Parsable e a v) => UnspreadLeft e a -> ParseOp m e a v
vertAllLefts evalLeft n (!tchart, !vchart) = do
  let -- left = n (and middle <= n)
      leftn = pmap (uncurry $ vertLeft evalLeft) $!! do -- in list monad
        left     <- tcGetByLength tchart n
        (i, top) <- vcGetByLeftChild n vchart (tRightSlice $ iItem left)
        pure (left, (top, i))

      -- middle = n (and left < n)
      vertn = pmap (uncurry $ vertLeft evalLeft) $!! do -- in list monad
        (i, top, lslice) <- vcGetByLengthLeft vchart n
        left             <- filter (\item -> transLen (iItem item) < n)
          $ tcGetByRight tchart lslice
        pure (left, (top, i))

      -- insert new transitions into chart
      tchart' = foldl' tcMerge (foldl' tcMerge tchart leftn) vertn
  return (tchart', vchart)

-- | Perform all right verts where either @r@ or @m@ have length @n@
vertAllRights
  :: (Monad m, Parsable e a v) => UnspreadRight e a -> ParseOp m e a v
vertAllRights evalRight n (!tchart, !vchart) = do
  let -- right = n (and middle <= n)
      !rightn = force $ pmap (uncurry $ vertRight evalRight) $!! do -- in list monad
        right <- tcGetByLength tchart n
        vert  <- vcGetByRightChild n vchart (tLeftSlice $ iItem right)
        pure (vert, right)

      -- middle = n (and left < n)
      !vertn = force $ pmap (uncurry $ vertRight evalRight) $!! do -- in list monad
        vert  <- vcGetByLength vchart n
        right <- filter (\i -> transLen (iItem i) < n)
          $ tcGetByLeft tchart (tRightSlice $ iItem $ vMiddle vert)
        pure (vert, right)

      -- insert new transitions into chart
      !tchart' = foldl' tcMerge (foldl' tcMerge tchart rightn) vertn
  return (tchart', vchart)

-- | perform all merges where either @l@ or @r@ have length @n@
mergeAll
  :: forall e a v m
   . (Monad m, Parsable e a v)
  => Unsplit e a v
  -> ParseOp m e a v
mergeAll mrg n (!tchart, !vchart) = do
  let !byLen = force $ tcGetByLength tchart n

      -- left = n (and right <= n)
      !leftn = pmap (uncurry (merge mrg)) $!! do
        left  <- byLen
        right <- filter (\r -> transLen (iItem r) <= n)
          $ tcGetByLeft tchart (tRightSlice $ iItem left)
        pure (left, right)

      -- right = n (and left < n)
      !rightn = pmap (uncurry (merge mrg)) $!! do
        right <- byLen
        left  <- filter (\l -> transLen (iItem l) < n)
          $ tcGetByRight tchart (tLeftSlice $ iItem right)
        pure (left, right)

      -- insert new transitions into chart
      !tchart' = foldl' tcMerge (foldl' tcMerge tchart leftn) rightn
  return (tchart', vchart)

-- parsing entry point
----------------------

-- | The main entrypoint to the parser.
-- Expects an evaluator for the specific grammar
-- and an input path.
-- Returns the combined semiring value of all full derivations.
parse
  :: Parsable e a v
  => (TChart e a v -> Either (VChart e a v) [Slice a] -> Int -> IO ())
  -> Eval e e' a a' v
  -> Path a' e'
  -> IO v
parse logCharts eval path = do
  logCharts tinit (Right $ pathNodes slicePath) 1
  (tfinal, vfinal) <- foldM
    (flip $ parseStep (\t v i -> logCharts t (Left v) i) eval)
    (tinit, vcEmpty len)
    [2 .. len - 1]
  logCharts tfinal (Left vfinal) len
  let goals = tcGetByLength tfinal len
  return $ R.sum $ S.getScoreVal . iValue <$> goals
 where
  wrapPath (Path a e rst) = Path (Inner a) (Just e) $ wrapPath rst
  wrapPath (PathEnd a   ) = Path (Inner a) Nothing $ PathEnd Stop
  path'     = Path Start Nothing $ wrapPath path
  len       = pathLen path'
  slicePath = mapNodesWithIndex
    0
    (\i notes -> Slice i (evalSlice eval <$> notes) i i)
    path'
  mkTrans l esurf r = mk <$> evalUnfreeze eval
                                          (sContent l)
                                          esurf
                                          (sContent r)
                                          (isStop $ sContent r)
    where mk (e, v) = Transition l e r False := S.val v
  trans0 = mapEdges mkTrans slicePath
  tinit  = tcMerge tcEmpty $ concat trans0

logSize
  :: TChart e1 a1 v1 -> Either (VChart e2 a2 v2) [Slice a2] -> Int -> IO ()
logSize tc vc n = do
  putStrLn $ "parsing level " <> show n
  putStrLn $ "transitions: " <> show (length $ tcGetByLength tc n)
  let nverts = case vc of
        Left  chart -> length $ vcGetByLength chart (n - 1)
        Right lst   -> length lst
  putStrLn $ "verts: " <> show nverts

parseSize :: Parsable e a v => Eval e e' a a' v -> Path a' e' -> IO v
parseSize = parse logSize

logNone :: Applicative f => p1 -> p2 -> p3 -> f ()
logNone _ _ _ = pure ()

parseSilent :: Parsable e a v => Eval e e' a a' v -> Path a' e' -> IO v
parseSilent = parse logNone

-- fancier logging
-- ---------------

printTikzSlice :: Show a => Slice a -> IO ()
printTikzSlice (Slice f sc sid l) = do
  putStrLn
    $  "    \\node[slice,align=center] (slice"
    <> show sid
    <> ") at ("
    <> show (fromIntegral (f + l) / 2.0)
    <> ",0) {"
    <> showTex sc
    <> "\\\\ "
    <> show sid
    <> "};"

printTikzVert neighbors (Vert top@(Slice f c i l) _ middle) = do
  let index      = f + l
      xpos       = fromIntegral (f + l) / 2.0
      ypos       = IM.findWithDefault 0 index neighbors
      neighbors' = IM.alter
        (\case
          Just n  -> Just (n + 1)
          Nothing -> Just 1
        )
        index
        neighbors
  putStrLn
    $  "    \\node[slice,align=center] (slice"
    <> show i
    <> ") at ("
    <> show xpos
    <> ","
    <> show ypos
    <> ") {"
    <> showTex c
    <> "\\\\ ("
    <> show (sID $ tLeftSlice $ iItem middle)
    <> ") - "
    <> show i
    <> " - ("
    <> show (sID $ tRightSlice $ iItem middle)
    <> ")};"
  pure neighbors'

printTikzTrans neighbors t@(Transition sl tc sr _) = do
  let tid        = "t" <> show (hash t)
      index      = sFirst sl + sLast sr
      xpos       = fromIntegral index / 2.0
      ypos       = IM.findWithDefault 0 index neighbors
      neighbors' = IM.alter
        (\case
          Just n  -> Just (n + 1)
          Nothing -> Just 1
        )
        index
        neighbors
  putStrLn
    $  "  \\begin{scope}[xshift="
    <> show xpos
    <> "cm,yshift="
    <> show ypos
    <> "cm]"
  putStrLn
    $  "    \\node[slice] ("
    <> tid
    <> "left) at (-0.1,0) {"
    <> show (sID sl)
    <> "};"
  putStrLn
    $  "    \\node[slice] ("
    <> tid
    <> "right) at (0.1,0) {"
    <> show (sID sr)
    <> "};"
  -- printTikzSlice sl (tid <> "left")  "(-0.2,0)"
  -- printTikzSlice sr (tid <> "right") "(0.2,0)"
  putStrLn
    $  "    \\draw[transition] ("
    <> tid
    <> "left) -- ("
    <> tid
    <> "right);"
  putStrLn "  \\end{scope}"
  pure neighbors'

logTikz tc vc n = do
  putStrLn $ "\n% level " <> show n
  let rel = if n <= 2
        then ""
        else ",shift={($(0,0 |- scope" <> show (n - 1) <> ".north)+(0,1cm)$)}"
  putStrLn $ "\\begin{scope}[local bounding box=scope" <> show n <> rel <> "]"
  putStrLn "  % verticalizations:"
  case vc of
    Left  chart -> foldM_ printTikzVert IM.empty $ vcGetByLength chart (n - 1)
    Right lst   -> mapM_ printTikzSlice lst
  putStrLn "\n  % transitions:"
  foldM_ printTikzTrans IM.empty $ iItem <$> tcGetByLength tc n
  putStrLn "\\end{scope}"
