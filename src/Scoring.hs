-- | Semiring scores lifted to functions that combine to the left and right.
-- This is used to express "partially applied" scores that occur
-- when the score of a verticalization is distributed to two parent edges.
-- The full score of the operation is restored when the two parent edges are eventually combined again.
module Scoring
  ( -- * The Score Type
    Score(..)
  , leftSide
  , rightSide
  , sides
  , score
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
  )
where

import qualified Data.Semiring                 as R

----------------
-- Score type --
----------------

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
  | SRight !i !(s -> s)
  -- ^ The right part of a combination, expects an argument to its left.
  -- Implemented as a function @fr :: s -> s@ that takes the value from the left.
  | SLeft !((s -> s) -> s) !i
  -- ^ The left part of a combination, expects an argument to its right.
  -- Implemented as a higher-order function @fl :: (s -> s) -> s@
  -- that takes the @fr@ function from its right and applies it to an internal value.
  -- In addition, @fl@ may modify the result of @fr@ by combining it with other @s@ values.
  | SBoth !i !((s -> s) -> s -> s) !i
  -- ^ A combination of 'SLeft' and 'SRight' that expects arguments on both sides.
  -- Implemented as a function @fb :: (s -> s) -> s -> s@.
  deriving ()

-- | Returns the ID on the left side of an 'Score',
-- or 'Nothing' for 'SVal' and 'SLeft'.
-- 
-- > a-b -> a
leftSide :: Score s i -> Maybe i
leftSide (SVal _     ) = Nothing
leftSide (SLeft  _ _ ) = Nothing
leftSide (SRight i _ ) = Just i
leftSide (SBoth i _ _) = Just i

-- | Returns the ID on the right side of an 'Score',
-- or 'Nothing' for 'SVal' and 'SRight'.
--
-- > a-b -> b
rightSide :: Score s i -> Maybe i
rightSide (SVal _     ) = Nothing
rightSide (SLeft  _ i ) = Just i
rightSide (SRight _ _ ) = Nothing
rightSide (SBoth _ _ i) = Just i

-- | Returns the signature of a 'Score',
-- i.e. its IDs (or 'Nothing') on both sides.
--
-- > a-b -> (a,b)
sides :: Score s i -> (Maybe i, Maybe i)
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
  show (SLeft  _ i   ) = "()-" <> show i
  show (SRight i _   ) = show i <> "-()"
  show (SBoth il _ ir) = show il <> "-" <> show ir

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
times (SVal s1) (SVal s2) = Just $ SVal $ s1 R.* s2
times (SLeft fl il) (SRight ir fr) | il == ir = Just $ SVal (fl fr)
-- creates right
times (SRight i fr) (SVal s) = Just $ SRight i (\l -> fr l R.* s)
times (SBoth il fb ir) (SRight i fr) | ir == i = Just $ SRight i (fb fr)
-- creates left
times (SVal s) (SLeft fl i) = Just $ SLeft (\r -> s R.* fl r) i
times (SLeft fl i) (SBoth il fb ir) | i == il = Just $ SLeft (fl . fb) ir
-- creates both
times (SRight il fr) (SLeft fl ir) = Just $ SBoth il (\r l -> fr l R.* fl r) ir
times (SBoth il fa ia) (SBoth ib fb ir) | ia == ib =
  Just $ SBoth il (fa . fb) ir
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
plus (SVal s1) (SVal s2) = Just $ SVal $ s1 R.+ s2
plus (SRight i fr1) (SRight i' fr2) | i == i' =
  Just $ SRight i $ \l -> fr1 l R.+ fr2 l
plus (SLeft fl1 i) (SLeft fl2 i') | i == i' =
  Just $ SLeft (\r -> fl1 r R.+ fl2 r) i
plus (SBoth il f1 ir) (SBoth il' f2 ir') | il == il' && ir == ir' =
  Just $ SBoth il (\r l -> f1 r l R.+ f2 r l) ir
plus _ _ = Nothing

-- | Checks if two 'Score's can be combined with 'times'.
--
-- > (a-b, c-d) -> b = c
compatible :: (Eq i) => Score s i -> Score s i -> Bool
compatible l r = rightSide l == leftSide r

-- | Checks if two 'Score's can be combined with 'plus'.
--
-- > (a-b, c-d) -> (a = c) ∧ (b = d)
similar :: (Eq i) => Score s i -> Score s i -> Bool
similar s1 s2 = sides s1 == sides s2

-----------
-- rules --
-----------

-- | Combines the 'Score's of two edges with a @split@ operation into the score of the parent edge.
--
-- > a-b   b-c
-- > --------- merge
-- >    a-c
mergeScores
  :: (R.Semiring s, Eq i)
  => s                 -- ^ The score of the split operation.
  -> Score s i         -- ^ The 'Score' of the left child edge.
  -> Score s i         -- ^ The 'Score' of the right child edge.
  -> Maybe (Score s i) -- ^ The 'Score' of the parent edge, if it exists.
mergeScores op left right = do
  children <- times left right
  times (SVal op) children

-- | Creates the 'Score' of a left parent edge from a left child edge of a @vert@.
vertScoresLeft
  :: (Eq i)
  => i                 -- ^ The new ID that marks both parent edges
  -> Score s i         -- ^ The 'Score' of the left child edge.
  -> Maybe (Score s i) -- ^ The 'Score' of the left parent edge, if it exists.
vertScoresLeft newid = wrap
 where
  -- wrap the left input score into a new layer with a new ID
  wrap (SVal val  ) = Just $ SLeft (\fr -> fr val) newid
  wrap (SLeft fl _) = Just $ SLeft fl newid
  wrap _            = Nothing

-- | Creates the 'Score' of a right parent edge
-- from the middle and right child edges of a @vert@
-- and a @horizontalize@ operation.
vertScoresRight
  :: (Eq i, R.Semiring s)
  => i                 -- ^ The new ID that marks both parent edges.
  -> s                 -- ^ The score of the @horizontalize@ operation.
  -> Score s i         -- ^ The 'Score' of the middle child edge.
  -> Score s i         -- ^ The 'Score' of the right child edge.
  -> Maybe (Score s i) -- ^ The 'Score' of the right parent edge, if it exists.
vertScoresRight newid op m r = do
  let op' = unwrap op $ leftSide m
  opm <- times op' m
  times opm r
 where
  -- generate a value on the right
  -- that consumes the left parent edge's value when supplied
  -- and combines with m on the right
  unwrap op Nothing  = SRight newid (\l -> op R.* l)
  unwrap op (Just i) = SBoth newid (\r l -> op R.* r l) i




-- -- TODO: generalize to l::a-b?
-- -- | Combines the 'Score's of three child edges with a @horizontalize@ operation
-- -- into the scores of the two parent edges, using a new ID to mark their scores as co-dependent.
-- -- The left child edge must be a 'SVal' or 'SLeft' in a left-most derivation.
-- --
-- -- > ()-b   b-c   c-d
-- -- > --------------------------------- vert
-- -- > left: ()-I
-- -- > right: I-b × b-c × c-d = I-d
-- vertScores
--   :: (R.Semiring s, Eq i)
--   => i         -- ^ The new ID that marks the resulting parent edges.
--   -> s         -- ^ The score of the @horizontalize@ operation.
--   -> Score s i -- ^ The 'Score' of the left child edge. Must be 'SVal' or 'SLeft'.
--   -> Score s i -- ^ The 'Score' of the middle child edge.
--   -> Score s i -- ^ The 'Score' of the right child edge.
--   -> Maybe (Score s i, Score s i) -- ^ The 'Score's of the parent edges, if they exist.
-- vertScores newid op l m r = do
--   left  <- wrap l
--   opl   <- unwrap op l
--   mr    <- times m r
--   right <- times opl mr
--   pure (left, right)
--  where
--   -- wrap the left input into a new layer with a new id
--   wrap (SVal val  ) = Just $ SLeft (\fr -> fr val) newid
--   wrap (SLeft fl _) = Just $ SLeft fl newid
--   wrap _            = Nothing
--   -- generate a value on the right that consumes the new left value when supplied
--   unwrap op (SVal _   ) = Just $ SRight newid (\l -> op R.* l)
--   unwrap op (SLeft _ i) = Just $ SBoth newid (\r l -> op R.* r l) i
--   unwrap _  _           = Nothing
