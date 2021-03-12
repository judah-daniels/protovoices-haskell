module Scoring where

-- | A partially applied score.
-- Comes in four variants:
-- SVal carries a fully applied value.
-- SLeft is the left part of a combination and expects a part to its right;
-- SRight is the right part of a combination and expects a part to its left;
-- SBoth can combine to both sides.
-- 
-- Each variant carries IDs that determine which objects fit on either of its sides.
-- Only score objects with matching IDs can be combined.
-- 
-- Partially applied values are represented as functions:
-- SRight carries a function @fr :: s -> s@ that is supposed to be applied to the value on its left.
-- SLeft represents this value, but as a function @fl :: (s -> s) -> s@
-- that takes the function fr from its right and applies it to its internal value.
-- This allows to add some additional information around whatever fr returns.
-- SBoth combines the two as a function @fb :: (s -> s) -> s -> s@
-- that expects a function on its right and a value on its left.
--
-- As a shorthand notation, we use @a-b@ to indicate a value
-- that depends on @a@ on its left and on @b@ on its right.
-- If the value does not depend on anything on either side, we use @()@,
-- i.e. @()-a@ stands for @SLeft _ a@ and @()-()@ stands for @SVal _@.
data Score s i = SVal !s
               | SLeft !((s -> s) -> s) !i
               | SRight !i !(s -> s)
               | SBoth !i !((s -> s) -> s -> s) !i
  deriving ()

leftSide :: Score s i -> Maybe i
leftSide (SVal _     ) = Nothing
leftSide (SLeft  _ _ ) = Nothing
leftSide (SRight i _ ) = Just i
leftSide (SBoth i _ _) = Just i

rightSide :: Score s i -> Maybe i
rightSide (SVal _     ) = Nothing
rightSide (SLeft  _ i ) = Just i
rightSide (SRight _ _ ) = Nothing
rightSide (SBoth _ _ i) = Just i

instance (Show i) => Show (Score s i) where
  show (SVal _       ) = "()-()"
  show (SLeft  _ i   ) = "()-" <> show i
  show (SRight i _   ) = show i <> "-()"
  show (SBoth il _ ir) = show il <> "-" <> show ir

-- | Combines two partially applied scores.
-- Shapes and IDs at the adjacent sides must match, otherwise @Nothing@ is returned.
-- @
--   a-b × b-c -> a-c
-- @
combine :: (Semigroup s, Eq i) => Score s i -> Score s i -> Maybe (Score s i)
-- creates value
combine (SVal s1) (SVal s2) = Just $ SVal $ s1 <> s2
combine (SLeft fl il) (SRight ir fr) | il == ir = Just $ SVal (fl fr)
-- creates right
combine (SRight i fr) (SVal s) = Just $ SRight i (\l -> fr l <> s)
combine (SBoth il fb ir) (SRight i fr) | ir == i = Just $ SRight i (fb fr)
-- creates left
combine (SVal s) (SLeft fl i) = Just $ SLeft (\r -> s <> fl r) i
combine (SLeft fl i) (SBoth il fb ir) | i == il = Just $ SLeft (fl . fb) ir
-- creates both
combine (SRight il fr) (SLeft fl ir) =
  Just $ SBoth il (\r l -> fr l <> fl r) ir
combine (SBoth il fa ia) (SBoth ib fb ir) | ia == ib =
  Just $ SBoth il (fa . fb) ir
-- otherwise
combine _ _ = Nothing

emptyScore :: (Monoid s) => Score s i
emptyScore = SVal mempty

-- | Checks if two scores can be combined.
-- (a-b, c-d) -> b==c
compatible :: (Eq i) => Score s i -> Score s i -> Bool
compatible l r = rightSide l == leftSide r

-----------
-- rules --
-----------

-- | Combines the score of two edges with a @split@ operation into the score of the parent edge.
-- @
--   a-b   b-c
--   --------- merge
--      a-c
-- @
mergeScores
  :: (Semigroup s, Eq i) => s -> Score s i -> Score s i -> Maybe (Score s i)
mergeScores op left right = do
  children <- combine left right
  combine (SVal op) children

-- TODO: generalize to l::a-b?
-- | Combines the scores of three child edges with a @horizontalize@ operation
-- into the scores of the two parent edges, using a new ID to mark their scores as co-dependent.
-- The left child edge must be a SVal or SLeft in a left-most derivation.
-- @
--   ()-b   b-c   c-d
--   ---------------------------------
--   left: ()-I
--   right: I-b × b-c × c-d = I-d
-- @
vertScores
  :: (Semigroup s, Eq i)
  => i
  -> s
  -> Score s i -- must be SVal or SLeft
  -> Score s i
  -> Score s i
  -> Maybe (Score s i, Score s i)
vertScores newid op l m r = do
  left  <- wrap l
  opl   <- unwrap op l
  mr    <- combine m r
  right <- combine opl mr
  pure (left, right)
 where
  -- wrap the left input into a new layer with a new id
  wrap (SVal val  ) = Just $ SLeft (\fr -> fr val) newid
  wrap (SLeft fl _) = Just $ SLeft fl newid
  wrap _            = Nothing
  -- generate a value on the right that consumes the new left value when supplied
  unwrap op (SVal _   ) = Just $ SRight newid (\l -> op <> l)
  unwrap op (SLeft _ i) = Just $ SBoth newid (\r l -> op <> r l) i
  unwrap _  _           = Nothing
