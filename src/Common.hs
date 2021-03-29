{-# LANGUAGE DeriveGeneric #-}
module Common where

-- StartStop
-- =========

import           GHC.Generics                   ( Generic )
import           Control.DeepSeq                ( NFData )
import qualified Data.Semiring                 as R

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
data Eval e e' a v = Eval
  { evalVertMiddle  :: VertMiddle e a v
  , evalVertLeft :: VertLeft e a v
  , evalVertRight :: VertRight e a v
  , evalMerge :: Merge e a v
  , evalThaw  :: StartStop a -> e' -> StartStop a -> [(e, v)]
  }

-- | Maps a function over all scores produced by the evaluator.
mapEvalScore :: (v -> w) -> Eval e e' a v -> Eval e e' a w
mapEvalScore f (Eval vm vl vr m t) = Eval vm' vl vr m' t'
 where
  vm' = fmap (fmap f) . vm
  m' sl l sm r sr is2nd = fmap f <$> m sl l sm r sr is2nd
  t' l e r = fmap f <$> t l e r

-- left-most derivation outer operations
-- =====================================

data Leftmost s f h = LMSplitLeft s
                    | LMFreeze f
                    | LMSplitRight s
                    | LMHorizontalize h
  deriving (Eq, Ord, Show)

-- useful semirings
-- ================

data Derivation a = Do a
                  | Or (Derivation a) (Derivation a)
                  | Then (Derivation a) (Derivation a)
                  | NoOp
                  | Cannot
  deriving (Eq, Ord, Show)

instance R.Semiring (Derivation a) where
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
