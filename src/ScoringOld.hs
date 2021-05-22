{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | __Warning: This module is broken, use 'ScoringFunTyped' instead!__
--
-- Semiring scores lifted to functions that combine to the left and right.
-- This is used to express "partially applied" scores that occur
-- when the score of a verticalization is distributed to two parent edges.
-- The full score of the operation is restored when the two parent edges are eventually combined again.
module ScoringOld
  ( -- * The Score Type
    Score(..)
  , val
  , LeftId(..)
  , RightId(..)
  , leftSide
  , rightSide
  , sides
  , -- * Semiring operations
    --
    -- Semiring operations can be lifted to partial scores,
    -- but since it is not guaranteed that their arguments can be combined,
    -- they are partial.
    times
  , plus
  , compatible
  , similar
  , -- * grammatical combinators
    --
    -- The following combinators correspond to the merge and vert operations
    -- of the path-graph grammar.
    mergeScores
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

-- | A partially applied score of type @s@.
-- Comes in four variants,
-- depending on whether the score is fully applied
-- or needs to be combined on either or both sides.
-- Values that need to be combined are represented as functions.
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
  | SRight !(LeftId i) !(s -> s)
  -- ^ The right part of a combination, expects an argument to its left.
  -- Implemented as a function @fr :: s -> s@ that takes the value from the left.
  | SLeft !((s -> s) -> s) !(RightId i)
  -- ^ The left part of a combination, expects an argument to its right.
  -- Implemented as a higher-order function @fl :: (s -> s) -> s@
  -- that takes the @fr@ function from its right and applies it to an internal value.
  -- In addition, @fl@ may modify the result of @fr@ by combining it with other @s@ values.
  | SBoth !(LeftId i) !((s -> s) -> s -> s) !(RightId i)
  -- ^ A combination of 'SLeft' and 'SRight' that expects arguments on both sides.
  -- Implemented as a function @fb :: (s -> s) -> s -> s@.
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

-- | Returns the signature of a 'Score',
-- i.e. its IDs (or 'Nothing') on both sides.
--
-- > a-b -> (a,b)
sides :: Score s i -> (Maybe (LeftId i), Maybe (RightId i))
sides (SVal _       ) = (Nothing, Nothing)
sides (SLeft  _ i   ) = (Nothing, Just i)
sides (SRight i _   ) = (Just i, Nothing)
sides (SBoth il _ ir) = (Just il, Just ir)

-- | Returns the value inside a score, if it is fully applied (i.e. 'SVal').
score :: Score s i -> Maybe s
score (SVal s) = Just s
score _        = Nothing

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

-- | Combines two partially applied 'Score's
-- by applying them to each other and/or multiplying the underlying semiring values.
-- Shapes and IDs at the adjacent sides must match, otherwise 'Nothing' is returned.
--
-- > a-b × b-c -> a-c
times
  :: (R.Semiring s, Eq i, Show i) => Score s i -> Score s i -> Maybe (Score s i)
-- creates value
times (SVal !s1) (SVal !s2) = Just $! SVal $! s1 R.* s2
times (SLeft !fl !il) (SRight !ir !fr) | il `match` ir = Just $! SVal $! fl fr
-- creates right
times (SRight i fr) (SVal s) = Just $! SRight i (\(!l) -> fr l R.* s)
times (SBoth il fb ir) (SRight i fr) | ir `match` i = Just $! SRight il (fb fr)
-- creates left
times (SVal s) (SLeft fl i) = Just $! SLeft (\(!r) -> s R.* fl r) i
times (SLeft fl i) (SBoth il fb ir) | i `match` il = Just $! SLeft (fl . fb) ir
-- creates both
times (SRight il fr) (SLeft fl ir) =
  Just $! SBoth il (\(!r) (!l) -> fr l R.* fl r) ir
times (SBoth il fa ia) (SBoth ib fb ir) | ia `match` ib =
  Just $! SBoth il (fa . fb) ir
-- otherwise
times _ _ = Nothing

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
  Just $! SRight i $ \(!l) -> fr1 l R.+ fr2 l
plus (SLeft fl1 i) (SLeft fl2 i') | i == i' =
  Just $! SLeft (\(!r) -> fl1 r R.+ fl2 r) i
plus (SBoth il f1 ir) (SBoth il' f2 ir') | il == il' && ir == ir' =
  Just $! SBoth il (\(!r) (!l) -> f1 r l R.+ f2 r l) ir
plus _ _ = Nothing

-- | Checks if two 'Score's can be combined with 'times'.
--
-- > (a-b, c-d) -> b = c
compatible :: (Eq i) => Score s i -> Score s i -> Bool
compatible l r = case (rightSide l, leftSide r) of
  (Nothing, Nothing) -> True
  (Just il, Just ir) -> il `match` ir
  _                  -> False

-- | Checks if two 'Score's can be combined with 'plus'.
--
-- > (a-b, c-d) -> (a = c) ∧ (b = d)
similar :: (Eq i) => Score s i -> Score s i -> Bool
similar s1 s2 = sides s1 == sides s2

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
  :: (R.Semiring s, Eq i, Show i)
  => s         -- ^ The score of the split operation.
  -> Score s i -- ^ The 'Score' of the left child edge.
  -> Score s i -- ^ The 'Score' of the right child edge.
  -> Score s i -- ^ The 'Score' of the parent edge, if it exists.
mergeScores op left right = fromMaybe err $ do
  children <- times left right
  times (adapt $ leftSide left) children
 where
  err =
    error
      $  "Attempting illegal merge: left="
      <> show left
      <> ", right="
      <> show right
  adapt Nothing = SVal op
  adapt (Just (LeftId i)) =
    SBoth (LeftId i) (\(!r) (!l) -> op R.* r l) (RightId i)

-- | Creates the 'Score' of a left parent edge from a left child edge of a @vert@.
-- Will throw an error if called on invalid input to indicate parser bugs.
vertScoresLeft
  :: (Eq i, Show i)
  => i         -- ^ The new ID that marks both parent edges
  -> Score s i -- ^ The 'Score' of the left child edge.
  -> Score s i -- ^ The 'Score' of the left parent edge, if it exists.
vertScoresLeft newid = wrap
 where
  -- wrap the left input score into a new layer with a new ID
  wrap (SVal val  ) = SLeft (\fr -> fr val) (RightId newid)
  wrap (SLeft fl _) = SLeft fl (RightId newid)
  wrap other        = error $ "Attempting illegal left-vert on " <> show other

-- | Creates the 'Score' of a right parent edge
-- from the middle and right child edges of a @vert@
-- and a @horizontalize@ operation.
vertScoresRight
  :: (Eq i, R.Semiring s, Show i)
  => i                 -- ^ The new ID that marks both parent edges.
  -> s                 -- ^ The score of the @horizontalize@ operation.
  -> Score s i         -- ^ The 'Score' of the middle child edge.
  -> Score s i         -- ^ The 'Score' of the right child edge.
  -> Score s i -- ^ The 'Score' of the right parent edge, if it exists.
vertScoresRight newid op m r = fromMaybe err $ do
  let op' = unwrap $ leftSide m
  opm <- times op' m
  times opm r
 where
  err =
    error $ "Attempting illegal left-vert: m=" <> show m <> ", r=" <> show r
  -- generate a value on the right
  -- that consumes the left parent edge's value when supplied
  -- and combines with m on the right
  unwrap Nothing = SRight (LeftId newid) (\l -> op R.* l)
  unwrap (Just (LeftId i)) =
    SBoth (LeftId newid) (\fr l -> op R.* fr l) (RightId i)
