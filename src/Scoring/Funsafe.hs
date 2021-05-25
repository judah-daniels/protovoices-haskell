{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}

-- | /This module is deprecated, use 'Scoring.FunTyped' instead./
--
-- Semiring scores with "holes".
-- Holes are used to express "partially applied" scores that occur
-- when the score of a verticalization is distributed to two parent edges.
-- The full score of the operation is restored when the two parent edges are eventually combined again.
--
-- This modules implements partial scores using /unsafe/ functions.
-- While the grammar combinators should fail with meaningful errors,
-- the primitive combinators can fail because of incomplete patterns.
module Scoring.Funsafe
  ( -- * The Score Type
    Score(..)
  , val
  , LeftId(..)
  , RightId(..)
  , leftSide
  , rightSide
  , showScore
  , -- * Semiring operations
    --
    -- Semiring operations can be lifted to partial scores,
    -- but since it is not guaranteed that their arguments can be combined,
    -- they are partial.
    times
  , plus
    -- * grammatical combinators
    --
    -- The following combinators correspond to the merge and vert operations
    -- of the path-graph grammar.
  , mergeScores
  , vertScoresLeft
  , vertScoresRight
  , addScores
  , getScoreVal
  )
where

import qualified Data.Semiring                 as R
import           GHC.Generics                   ( Generic )
import           Control.DeepSeq                ( NFData )
import           Data.Hashable                  ( Hashable )
import           Data.Maybe                     ( fromMaybe )

----------------
-- Score type --
----------------

newtype LeftId i = LeftId i
  deriving (Eq, Ord, Generic, NFData, Hashable)

instance Show i => Show (LeftId i) where
  show (LeftId i) = show i

newtype RightId i = RightId i
  deriving (Eq, Ord, Generic, NFData, Hashable)

instance Show i => Show (RightId i) where
  show (RightId i) = show i

match :: Eq a => RightId a -> LeftId a -> Bool
match (RightId ir) (LeftId il) = il == ir

data RightHole s = RightHole !(RightHoles s)
                 | RightEnd !s
  deriving (Generic, NFData)

type RightHoles s = s -> RightHole s

type LeftHoles s = (RightHoles s -> s)

type BothHoles s = (RightHoles s -> RightHoles s)

-- | A partially applied score of type @s@.
-- Comes in four variants,
-- depending on whether the score is fully applied
-- or needs to be combined on either or both sides.
-- Values that need to be combined are lists that represent scores with holes.
-- Each variant carries IDs of type @i@ that determine which objects fit on either of its sides.
-- Only score objects with matching IDs can be combined.
-- 
-- As a shorthand notation, we use @a-b@ to indicate a value
-- that depends on @a@ on its left and on @b@ on its right.
-- If the value does not depend on anything on either side, we use @()@,
-- i.e. @()-a@ stands for @SLeft _ a@ and @()-()@ stands for @SVal _@.
data Score s i
  = SVal !s
  -- ^ Carries a fully applied value
  | SRight !(LeftId i) !(RightHoles s)
  -- ^ The right part of a combination, expects an argument to its left.
  -- Implemented as a function that takes a left counterpart
  -- and returns a score with fewer holes.
  | SLeft !(LeftHoles s) !(RightId i)
  -- ^ The left part of a combination, expects an argument to its right.
  -- Implemented as a function that takes a right hole and applies it.
  | SBoth !(LeftId i) !(BothHoles s) !(RightId i)
  -- ^ A combination of 'SLeft' and 'SRight' that expects arguments on both sides.
  -- Implemented as a function that expects both a right and a left hole to be complete.
  deriving (Generic, NFData)

-- | Creates a simple value score of type ()-().
val :: s -> Score s i
val = SVal

-- | Returns the ID on the left side of an 'Score',
-- or 'Nothing' for 'SVal' and 'SLeft'.
-- 
-- > a-b -> a
leftSide :: Score s i -> Maybe (LeftId i)
leftSide (SVal _     ) = Nothing
leftSide (SLeft  _ _ ) = Nothing
leftSide (SRight i _ ) = Just i
leftSide (SBoth i _ _) = Just i

-- | Returns the ID on the right side of an 'Score',
-- or 'Nothing' for 'SVal' and 'SRight'.
--
-- > a-b -> b
rightSide :: Score s i -> Maybe (RightId i)
rightSide (SVal _     ) = Nothing
rightSide (SLeft  _ i ) = Just i
rightSide (SRight _ _ ) = Nothing
rightSide (SBoth _ _ i) = Just i

instance (Show i) => Show (Score s i) where
  show (SVal _       ) = "()-()"
  show (SLeft  _  ir ) = "()-" <> show ir
  show (SRight il _  ) = show il <> "-()"
  show (SBoth il _ ir) = show il <> "-" <> show ir

showScore :: (Show s, Show i) => Score s i -> String
showScore (SVal v       ) = show v
showScore (SLeft  _  ir ) = "()-" <> show ir
showScore (SRight il _  ) = show il <> "-()"
showScore (SBoth il _ ir) = show il <> "-" <> show ir

-------------------------
-- semiring operations --
-------------------------

appendRight :: R.Semiring s => s -> RightHoles s -> RightHoles s
appendRight !s' fr !l = app $ fr l
 where
  app (RightEnd  r  ) = RightEnd $ r R.* s'
  app (RightHole fr') = RightHole $ appendRight s' fr'

prependLeft :: R.Semiring s => s -> LeftHoles s -> LeftHoles s
prependLeft !s fl = (s R.*) . fl

prependRight :: R.Semiring s => s -> RightHoles s -> RightHoles s
prependRight !s fr !l = prep $ fr l
 where
  prep (RightEnd  r  ) = RightEnd $ s R.* r
  prep (RightHole fr') = RightHole $ prependRight s fr'
-- prependRight s (RightHole fr) = RightHole $ fmap (prependRight s) . fr

-- mkRightHoles :: R.Semiring s => [s] -> RightHoles s
-- mkRightHoles = foldr addHoleRight (RightEnd R.one)

-- | Combines two partially applied 'Score's
-- by applying them to each other and/or multiplying the underlying semiring values.
-- Shapes and IDs at the adjacent sides must match, otherwise 'Nothing' is returned.
--
-- > a-b × b-c -> a-c
times
  :: (R.Semiring s, Eq i, Show i) => Score s i -> Score s i -> Maybe (Score s i)
-- creates value
times (SVal s1) (SVal s2) = Just $! SVal $ s1 R.* s2
times (SLeft fl il) (SRight ir fr) | il `match` ir = Just $! SVal $ fl fr
-- creates right
times (SRight i r) (SVal s) = Just $! SRight i $ appendRight s r
times (SBoth il fb ir) (SRight i fr) | ir `match` i = Just $ SRight il $ fb fr
-- creates left
times (SVal s) (SLeft fl i) = Just $! SLeft (prependLeft s fl) i
times (SLeft fl i) (SBoth il fb ir) | i `match` il = Just $! SLeft (fl . fb) ir
-- creates both
times (SRight il fr) (SLeft fl ir) =
  Just $! SBoth il ((`appendRight` fr) . fl) ir
times (SBoth il fa ia) (SBoth ib fb ir) | ia `match` ib =
  Just $! SBoth il (fa . fb) ir
-- otherwise
times _ _ = Nothing

rhplus :: (R.Semiring s) => RightHoles s -> RightHoles s -> RightHoles s
rhplus r1 r2 !l = case (r1 l, r2 l) of
  (RightEnd  e1, RightEnd e2 ) -> RightEnd $ e1 R.+ e2
  (RightHole f1, RightHole f2) -> RightHole $ rhplus f1 f2

-- | Adds two partially applied 'Score's
-- by adding their underlying (or resulting) semiring values.
-- This operation is only admitted
-- if the two scores are of the same shape and have matching IDs.
-- Otherwise, 'Nothing' is returned.
--
-- > a-b + a-b -> a-b
plus :: (R.Semiring s, Eq i) => Score s i -> Score s i -> Maybe (Score s i)
plus (SVal s1) (SVal s2) = Just $! SVal $ s1 R.+ s2
plus (SRight i fr1) (SRight i' fr2) | i == i' =
  Just $! SRight i $ rhplus fr1 fr2
plus (SLeft fl1 i) (SLeft fl2 i') | i == i' =
  Just $! SLeft (\r -> fl1 r R.+ fl2 r) i
plus (SBoth il bs1 ir) (SBoth il' bs2 ir') | il == il' && ir == ir' =
  Just $! SBoth il (\r -> rhplus (bs1 r) (bs2 r)) ir
plus _ _ = Nothing

-- helpers for constructing holes
-- ------------------------------

addHoleLeft :: s -> LeftHoles s -> LeftHoles s
addHoleLeft !s fl fr = case fr s of
  RightHole fr' -> fl fr'

mkLeftHole :: s -> LeftHoles s
mkLeftHole !s fr = case fr s of
  RightEnd v -> v

addHoleRight :: R.Semiring s => s -> RightHoles s -> RightHoles s
addHoleRight !r !rh !l = RightHole $ prependRight (l R.* r) rh

mkRightHole :: R.Semiring s => s -> RightHoles s
mkRightHole !r !l = RightEnd $ l R.* r

-----------
-- rules --
-----------

-- | Extracts the value from a fully applied 'Score'.
-- This function is intended to be used to extract the final score of the parser.
-- If the score is not fully applied,
-- throws an exception to indicate parser bugs.
getScoreVal :: Score s i -> s
getScoreVal (SVal s) = s
getScoreVal _        = error "cannot get value from partial score"

-- | Adds two 'Score's that are alternative derivations of the same transition.
-- This is expected to be called on compatible scores
-- and will throw an error otherwise to indicate parser bugs.
--
-- > a-b   a-b
-- > --------- add
-- >    a-b
addScores :: (R.Semiring s, Eq i) => Score s i -> Score s i -> Score s i
addScores a b = fromMaybe (error "illegal times") $ plus a b

-- | Combines the 'Score's of two edges with a @split@ operation into the score of the parent edge.
-- This is expected to be called on compatible scores
-- and will throw an error otherwise to indicate parser bugs.
--
-- > a-b   b-c
-- > --------- merge
-- >    a-c
mergeScores
  :: (R.Semiring s, Eq i, Show i, Show s)
  => s         -- ^ The score of the split operation.
  -> Score s i -- ^ The 'Score' of the left child edge.
  -> Score s i -- ^ The 'Score' of the right child edge.
  -> Score s i -- ^ The 'Score' of the parent edge, if it exists.
mergeScores op left right = fromMaybe err $ times left' right
 where
  err =
    error
      $  "Attempting illegal merge: left="
      <> show left
      <> ", right="
      <> show right
  left' = case left of
    SVal s         -> SVal (op R.* s)
    SLeft  fl i    -> SLeft (prependLeft op fl) i
    SRight i  rs   -> SRight i (prependRight op rs)
    SBoth il bs ir -> SBoth il (prependRight op . bs) ir

-- | Creates the 'Score' of a left parent edge from a left child edge of a @vert@.
-- Will throw an error if called on invalid input to indicate parser bugs.
vertScoresLeft
  :: (Eq i, Show i, R.Semiring s, Show s)
  => i         -- ^ The new ID that marks both parent edges
  -> Score s i -- ^ The 'Score' of the left child edge.
  -> Score s i -- ^ The 'Score' of the left parent edge, if it exists.
vertScoresLeft newid = wrap
 where
  newir = RightId newid
  -- wrap the left input score into a new layer with a new ID
  wrap (SVal v    ) = SLeft (addHoleLeft R.one $ mkLeftHole v) newir
  wrap (SLeft fl _) = SLeft (addHoleLeft R.one fl) newir
  wrap other        = error $ "Attempting illegal left-vert on " <> show other

-- | Creates the 'Score' of a right parent edge
-- from the middle and right child edges of a @vert@
-- and a @horizontalize@ operation.
vertScoresRight
  :: (Eq i, R.Semiring s, Show i, Show s)
  => i                 -- ^ The new ID that marks both parent edges.
  -> s                 -- ^ The score of the @horizontalize@ operation.
  -> Score s i         -- ^ The 'Score' of the middle child edge.
  -> Score s i         -- ^ The 'Score' of the right child edge.
  -> Score s i -- ^ The 'Score' of the right parent edge, if it exists.
vertScoresRight newid op m r = fromMaybe err $ do
  mr <- times m r
  pure $ unwrap mr
 where
  err =
    error $ "Attempting illegal right-vert: m=" <> show m <> ", r=" <> show r
  -- generate a value on the right
  -- that consumes the left parent edge's value when supplied
  -- and combines with m on the right
  newil = LeftId newid
  unwrap (SVal s     ) = SRight newil $ addHoleRight op $ mkRightHole s -- [op, s]
  unwrap (SRight _ rs) = SRight newil (addHoleRight op rs)
  unwrap (SLeft fl ir) =
    SBoth newil ((`appendRight` addHoleRight op (mkRightHole R.one)) . fl) ir
  unwrap (SBoth _ fb ir) = SBoth newil (addHoleRight op . fb) ir
