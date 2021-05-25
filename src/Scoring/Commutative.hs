{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | __Warning: This module only works with commutative semirings!__
-- For other use cases, use 'Scoring.FunTyped'.
--
-- Semiring scores with "holes".
-- Holes are used to express "partially applied" scores that occur
-- when the score of a verticalization is distributed to two parent edges.
-- The full score of the operation is restored when the two parent edges are eventually combined again.
--
-- This module implements partial scores as typesafe functions with phantom types
-- that indicate the number of holes on each side.
-- The grammatical combinators use an existential type 'Score'
-- that reifies the phantom types as singletons,
-- which allows different scores to be easily stored together
-- and is compatible with the 'Score' types from the other Scoring* modules.
-- Thus, the grammatical combinators are partial
-- and fail when used with incompatible scores, indicating a parser bug.
module Scoring.Commutative
  ( -- * The Score Type
    Score
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

newtype LeftId i = LeftId { runLeftId :: i }
  deriving (Eq, Ord, Generic, NFData, Hashable)

instance Show i => Show (LeftId i) where
  show (LeftId i) = show i

newtype RightId i = RightId { runRightId :: i }
  deriving (Eq, Ord, Generic, NFData, Hashable)

instance Show i => Show (RightId i) where
  show (RightId i) = show i

-- | A partially applied score of type @s@.
-- This version of partial scores ignores the order of operations
-- and thus only works for semirings where multiplication is commutative
-- 
-- As a shorthand notation, we use @a-b@ to indicate a value
-- that depends on @a@ on its left and on @b@ on its right.
-- If the value does not depend on anything on either side, we use @()@,
-- i.e. @()-a@ stands for @SLeft _ a@ and @()-()@ stands for @SVal _@.
data Score s i = SVal !(Maybe (LeftId i)) !s !(Maybe (RightId i))
  deriving (Generic, NFData)

-- | Creates a simple value score of type ()-().
val :: s -> Score s i
val v = SVal Nothing v Nothing

-- | Returns the ID on the left side of an 'Score',
-- or 'Nothing' for 'SVal' and 'SLeft'.
-- 
-- > a-b -> a
leftSide :: Score s i -> Maybe (LeftId i)
leftSide (SVal l _ _) = l

-- | Returns the ID on the right side of an 'Score',
-- or 'Nothing' for 'SVal' and 'SRight'.
--
-- > a-b -> b
rightSide :: Score s i -> Maybe (RightId i)
rightSide (SVal _ _ r) = r

showSide :: (Show i) => Maybe i -> String
showSide Nothing  = "()"
showSide (Just x) = show x

instance (Show i) => Show (Score s i) where
  show (SVal l _ r) = showSide l <> "-" <> showSide r

showScore :: (Show s, Show i) => Score s i -> String
showScore (SVal l v r) = showSide l <> "-" <> show v <> "-" <> showSide r

-------------------------
-- semiring operations --
-------------------------

-- | Combines two partially applied 'Score's
-- by applying them to each other and/or multiplying the underlying semiring values.
-- Shapes and IDs at the adjacent sides must match, otherwise 'Nothing' is returned.
--
-- > a-b × b-c -> a-c
times :: (R.Semiring s, Eq i) => Score s i -> Score s i -> Maybe (Score s i)
-- creates value
times (SVal l1 s1 r1) (SVal l2 s2 r2)
  | (runRightId <$> r1) == (runLeftId <$> l2) = Just $! SVal l1 (s1 R.* s2) r2
times _ _ = Nothing

-- | Adds two partially applied 'TypedScore's
-- by adding their underlying (or resulting) semiring values.
-- This operation is only admitted
-- if the two scores are of the same shape and have matching IDs.
-- Otherwise, 'Nothing' is returned.
--
-- > a-b + a-b -> a-b
plus :: (R.Semiring s, Eq i) => Score s i -> Score s i -> Maybe (Score s i)
plus (SVal l1 s1 r1) (SVal l2 s2 r2) | l1 == l2 && r1 == r2 =
  Just $! SVal l1 (s1 R.+ s2) r2
plus _ _ = Nothing

-----------
-- rules --
-----------

-- | Extracts the value from a fully applied 'Score'.
-- This function is intended to be used to extract the final score of the parser.
-- If the score is not fully applied,
-- throws an exception to indicate parser bugs.
getScoreVal :: Score s i -> s
getScoreVal (SVal Nothing s Nothing) = s
getScoreVal _ = error "cannot get value from partial score"

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
mergeScores op left right = fromMaybe err $ times (prep left) right
 where
  err =
    error
      $  "Attempting illegal merge: left="
      <> show left
      <> ", right="
      <> show right
  prep (SVal il s ir) = SVal il (op R.* s) ir

-- | Creates the 'Score' of a left parent edge from a left child edge of a @vert@.
-- Will throw an error if called on invalid input to indicate parser bugs.
vertScoresLeft
  :: (Eq i, Show i, R.Semiring s, Show s)
  => i         -- ^ The new ID that marks both parent edges
  -> Score s i -- ^ The 'Score' of the left child edge.
  -> Score s i -- ^ The 'Score' of the left parent edge, if it exists.
vertScoresLeft newid = wrap
 where
  -- wrap the left input score into a new layer with a new ID
  newir = RightId newid
  wrap (SVal il v _) = SVal il v (Just newir)

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
  unwrap (SVal _ s ir) = SVal (Just newil) (op R.* s) ir
