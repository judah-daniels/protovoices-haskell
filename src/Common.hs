{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Common where

-- StartStop
-- =========

import           GHC.Generics                   ( Generic )
import           Control.DeepSeq                ( NFData )

import qualified Data.Semiring                 as R
import qualified Data.Set                      as S
import           Data.Foldable                  ( foldl'
                                                , foldlM
                                                )

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

-- | Returns the content of an 'Inner', or 'Nothing'.
getInner :: StartStop a -> Maybe a
getInner (Inner a) = Just a
getInner _         = Nothing

isInner (Inner a) = True
isInner _         = False

isStart (:⋊) = True
isStart _    = False

isStop (:⋉) = True
isStop _    = False

distStartStop :: StartStop (a, b) -> (StartStop a, StartStop b)
distStartStop (:⋊)           = ((:⋊), (:⋊))
distStartStop (:⋉)           = ((:⋉), (:⋉))
distStartStop (Inner (a, b)) = (Inner a, Inner b)

-- evaluator interface
-- ===================

-- | An evaluator for verticalizations.
-- Returns the verticalization of a (middle) transition, if possible.
type VertMiddle e a v = (a, e, a) -> Maybe (a, v)

-- | An evaluator returning the possible left parent edges of a verticalization.
type VertLeft e a v = (e, a) -> a -> [e]

-- | An evaluator returning the possible right parent edges of a verticalization.
type VertRight e a v = (a, e) -> a -> [e]

-- | An evaluator for merges.
-- Returns possible merges of a given pair of transitions.
type Merge e a v = StartStop a -> e -> a -> e -> StartStop a -> Bool -> [(e, v)]

-- | A combined evaluator for verticalizations, merges, and thaws.
-- Additionally, contains a function for mapping terminal slices to semiring values.
data Eval e e' a a' v = Eval
  { evalVertMiddle  :: VertMiddle e a v
  , evalVertLeft :: VertLeft e a v
  , evalVertRight :: VertRight e a v
  , evalMerge :: Merge e a v
  , evalThaw  :: StartStop a -> e' -> StartStop a -> [(e, v)]
  , evalSlice :: a' -> a
  }

-- | Maps a function over all scores produced by the evaluator.
mapEvalScore :: (v -> w) -> Eval e e' a a' v -> Eval e e' a a' w
mapEvalScore f (Eval vm vl vr m t s) = Eval vm' vl vr m' t' s
 where
  vm' = fmap (fmap f) . vm
  m' sl l sm r sr is2nd = fmap f <$> m sl l sm r sr is2nd
  t' l e r = fmap f <$> t l e r

-- product evaluators
-- ------------------

productEval
  :: Eval e1 e' a1 a' v1
  -> Eval e2 e' a2 a' v2
  -> Eval (e1, e2) e' (a1, a2) a' (v1, v2)
productEval (Eval vertm1 vertl1 vertr1 merge1 thaw1 slice1) (Eval vertm2 vertl2 vertr2 merge2 thaw2 slice2)
  = Eval vertm vertl vertr merge thaw slice
 where
  vertm ((l1, l2), (m1, m2), (r1, r2)) = do
    (a, va) <- vertm1 (l1, m1, r1)
    (b, vb) <- vertm2 (l2, m2, r2)
    pure ((a, b), (va, vb))
  vertl ((l1, l2), (c1, c2)) (t1, t2) = do
    a <- vertl1 (l1, c1) t1
    b <- vertl2 (l2, c2) t2
    pure (a, b)
  vertr ((c1, c2), (r1, r2)) (t1, t2) = do
    a <- vertr1 (c1, r1) t1
    b <- vertr2 (c2, r2) t2
    pure (a, b)
  merge sl (tl1, tl2) (sm1, sm2) (tr1, tr2) sr is2nd = do
    (a, va) <- merge1 sl1 tl1 sm1 tr1 sr1 is2nd
    (b, vb) <- merge2 sl2 tl2 sm2 tr2 sr2 is2nd
    pure ((a, b), (va, vb))
   where
    (sl1, sl2) = distStartStop sl
    (sr1, sr2) = distStartStop sr
  thaw l e r = do
    (a, va) <- thaw1 l1 e r1
    (b, vb) <- thaw2 l2 e r2
    pure ((a, b), (va, vb))
   where
    (l1, l2) = distStartStop l
    (r1, r2) = distStartStop r
  slice s = (slice1 s, slice2 s)

-- restricting branching
-- ---------------------

newtype RightBranchHori = RB Bool
  deriving (Eq, Ord, Show)

evalRightBranchHori :: Eval RightBranchHori e' () a' ()
evalRightBranchHori = Eval vertm vertl vertr merge thaw slice
 where
  vertm (_, RB False, _) = Nothing
  vertm (_, RB True , _) = Just ((), ())
  vertl _ _ = [RB True]
  vertr _ _ = [RB False]
  merge _ _ _ _ _ _ = [(RB True, ())]
  thaw _ _ _ = [(RB True, ())]
  slice _ = ()

rightBranchHori
  :: Eval e2 e' a2 a' w -> Eval (RightBranchHori, e2) e' ((), a2) a' w
rightBranchHori = mapEvalScore snd . productEval evalRightBranchHori

-- restricting derivation order
-- ----------------------------

newtype Merged = Merged Bool
  deriving (Eq, Ord, Show)

evalSplitBeforeHori :: (Eval Merged e' () a' ())
evalSplitBeforeHori = Eval vertm vertl vertr merge thaw slice
 where
  vertm _ = Just ((), ())
  vertl (Merged True , _) _ = []
  vertl (Merged False, _) _ = [Merged False]
  vertr (_, Merged True ) _ = []
  vertr (_, Merged False) _ = [Merged False]
  merge _ _ _ _ _ _ = [(Merged True, ())]
  thaw _ _ _ = [(Merged False, ())]
  slice _ = ()

splitFirst :: Eval e2 e' a2 a' w -> Eval (Merged, e2) e' ((), a2) a' w
splitFirst = mapEvalScore snd . productEval evalSplitBeforeHori

-- left-most derivation outer operations
-- =====================================

data Leftmost s f h = LMSplitLeft s
                    | LMFreeze f
                    | LMSplitRight s
                    | LMHorizontalize h
  deriving (Eq, Ord, Show)

-- useful semirings
-- ================

data Derivations a = Do a
                   | Or (Derivations a) (Derivations a)
                   | Then (Derivations a) (Derivations a)
                   | NoOp
                   | Cannot
  deriving (Eq, Ord, Show)

instance R.Semiring (Derivations a) where
  zero = Cannot
  one  = NoOp
  plus Cannot a      = a
  plus a      Cannot = a
  plus a      b      = Or a b
  times Cannot _      = Cannot
  times _      Cannot = Cannot
  times NoOp   a      = a
  times a      NoOp   = NoOp
  times a      b      = Then a b

mapDerivations :: (R.Semiring r) => (a -> r) -> Derivations a -> r
mapDerivations f (Do a)     = f a
mapDerivations f NoOp       = R.one
mapDerivations f Cannot     = R.zero
mapDerivations f (Or   a b) = mapDerivations f a R.+ mapDerivations f b
mapDerivations f (Then a b) = mapDerivations f a R.* mapDerivations f b

flattenDerivations :: Ord a => Derivations a -> S.Set [a]
flattenDerivations = mapDerivations (\a -> S.singleton [a])
