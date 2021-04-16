{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module PVGrammar where

import           Common
import           Display

import           Musicology.Pitch

import qualified Data.Set                      as S
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.MultiSet                 as MS
import           Data.Foldable                  ( toList
                                                , foldl'
                                                )
import           Control.Monad                  ( foldM
                                                , mzero
                                                )
import           Data.Either                    ( partitionEithers )
import           Data.Maybe                     ( catMaybes
                                                , maybeToList
                                                )
-- element types
-- =============

-- slice type: sets of notes
----------------------------

-- | The content type of 'Slice's.
-- Contains a multiset of pitches, representing the notes in a slice.
newtype Notes i = Notes (MS.MultiSet (Pitch i))
  deriving (Eq, Ord)

instance (Notation (Pitch i)) => Show (Notes i) where
  show (Notes ns) =
    "{" <> L.intercalate "," (showNote <$> MS.toOccurList ns) <> "}"
   where
    showNote (p, n) = showNotation p <> mult
      where mult = if n /= 1 then "×" <> show n else ""

-- | Return the notes or start/stop symbols inside a slice.
-- This is useful to get all objects that an 'Edge' can connect to. 
innerNotes :: StartStop (Notes i) -> [StartStop (Pitch i)]
innerNotes (Inner (Notes n)) = Inner <$> MS.distinctElems n
innerNotes (:⋊)              = [(:⋊)]
innerNotes (:⋉)              = [(:⋉)]

-- transition type: sets of obligatory edges
--------------------------------------------

-- TODO: could this be improved to forbid start/stop symbols on the wrong side?
-- | A proto-voice edge between two nodes (i.e. notes or start/stop symbols).
type Edge i = (StartStop (Pitch i), StartStop (Pitch i))

-- | A proto-voice edge between two notes (excluding start/stop symbols).
type InnerEdge i = (Pitch i, Pitch i)

-- | The content type of 'Transition's.
-- Contains a multiset of normal (terminal) edges and a multiset of (non-terminal) passing edges.
-- The represented edges are those that are definitely used later on.
-- Edges that are not used are dropped before creating a child transition.
-- A transition that contains passing edges cannot be frozen.
data Edges i = Edges
               { edgesT  :: !(MS.MultiSet (Edge i))      -- ^ the terminal edges
               , edgesNT :: !(MS.MultiSet (InnerEdge i)) -- ^ the non-terminal edges
               }
  deriving (Eq, Ord)

instance (Notation (Pitch i)) => Show (Edges i) where
  show (Edges ts nts) = "{" <> L.intercalate "," (tts <> tnts) <> "}"
   where
    tts  = showT <$> MS.toOccurList ts
    tnts = showNT <$> MS.toOccurList nts
    showT ((p1, p2), n) = showNode p1 <> "-" <> showNode p2 <> "×" <> show n
    showNT ((p1, p2), n) =
      showNotation p1 <> ">" <> showNotation p2 <> "×" <> show n
    showNode (:⋊)      = "⋊"
    showNode (:⋉)      = "⋉"
    showNode (Inner p) = showNotation p

-- operations
-- ==========

-- | Marks different types of ornaments in the derivation.
data Ornament = FullNeighbor
              | LeftNeighborOfRight
              | RightNeighborOfLeft
              | FullRepeat
              | LeftRepeatOfRight
              | RightRepeatOfLeft
              | Passing
              | RootNote
  deriving (Eq, Ord, Show)

data LeftOrnament = SingleLeftNeighbor
                  | SingleLeftRepeat
  deriving (Eq, Ord, Show)

data RightOrnament = SingleRightNeighbor
                   | SingleRightRepeat
  deriving (Eq, Ord, Show)

isRepetitionOnLeft FullRepeat        = True
isRepetitionOnLeft RightRepeatOfLeft = True
isRepetitionOnLeft _                 = False

isRepetitionOnRight FullRepeat        = True
isRepetitionOnRight LeftRepeatOfRight = True
isRepetitionOnRight _                 = False

-- | Encodes the decisions made in a split operation.
-- Contains a list of elaborations for every parent edge.
-- Each elaboration contains the child pitch,
-- the corresponding ornament, and flags for whether to keep either of the child edges.
data Split i = SplitOp
  { splitTs :: !(M.Map (Edge i) [(Pitch i, Ornament, Bool, Bool)])
  , splitNTs :: !(M.Map (InnerEdge i) [(Pitch i, Bool, Bool)])
  , fromLeft :: !(M.Map (Pitch i) [(Pitch i, RightOrnament, Bool)])
  , fromRight :: !(M.Map (Pitch i) [(Pitch i, LeftOrnament, Bool)])
  }
  deriving (Eq, Ord)

instance (Notation (Pitch i)) => Show (Split i) where
  show (SplitOp ts nts ls rs) =
    "ts:{"
      <> opTs
      <> "}, nts:{"
      <> opNTs
      <> "}, ls:{"
      <> opLs
      <> "}, rs:{"
      <> opRs
      <> "}"
   where
    opTs  = L.intercalate "," (showT <$> M.toList ts)
    opNTs = L.intercalate "," (showNT <$> M.toList nts)
    opLs  = L.intercalate "," (showL <$> M.toList ls)
    opRs  = L.intercalate "," (showR <$> M.toList rs)
    showTEdge (n1, n2) = showNode n1 <> "-" <> showNode n2
    showTChild (p, o, l, r) = showNotation p <> ":" <> show (o, l, r)
    showT (e, cs) =
      showTEdge e <> "=>[" <> L.intercalate "," (showTChild <$> cs) <> "]"
    showNTEdge (n1, n2) = showNotation n1 <> ">" <> showNotation n2
    showNTChild (p, l, r) = showNotation p <> ":" <> show (Passing, l, r)
    showNT (e, cs) =
      showNTEdge e <> "=>[" <> L.intercalate "," (showNTChild <$> cs) <> "]"
    showSingleChild (c, o, k) = showNotation c <> ":" <> show (o, k)
    showL (p, ls) =
      showNotation p
        <> "=>["
        <> L.intercalate "," (showSingleChild <$> ls)
        <> "]"
    showR (p, rs) =
      "["
        <> L.intercalate "," (showSingleChild <$> rs)
        <> "]<="
        <> showNotation p
    showNode (:⋊)      = "⋊"
    showNode (:⋉)      = "⋉"
    showNode (Inner p) = showNotation p

instance (Ord i) => Semigroup (Split i) where
  (SplitOp ta nta la ra) <> (SplitOp tb ntb lb rb) = SplitOp (ta <+> tb)
                                                             (nta <+> ntb)
                                                             (la <+> lb)
                                                             (ra <+> rb)
   where
    (<+>) :: (Ord k, Semigroup a) => M.Map k a -> M.Map k a -> M.Map k a
    (<+>) = M.unionWith (<>)

instance (Ord i) => Monoid (Split i) where
  mempty = SplitOp M.empty M.empty M.empty M.empty

-- | Represents a freeze operation.
-- Since this just ties all remaining edges
-- (which must all be repetitions)
-- no decisions have to be encoded.
data Freeze = FreezeOp
  deriving (Eq, Ord)

instance Show Freeze where
  show _ = "()"

-- | Encodes the distribution of a pitch in a horizontalization.
-- 
-- All instances of a pitch must be either moved completely to the left or the right (or both).
-- In addition, some instances may be repeated on the other side.
-- The difference is indicated by the field of the 'ToLeft' and 'ToRight' constructors.
-- For example, @ToLeft 3@ indicates that out of @n@ instances,
-- all @n@ are moved to the left and @n-3@ are replicated on the right.
data HoriDirection = ToLeft !Int  -- ^ all to the left, n fewer to the right
                   | ToRight !Int -- ^ all to the right, n fewer to the left
                   | ToBoth      -- ^ all to both
  deriving (Eq, Ord, Show)

-- | Represents a horzontalization operation.
-- Records for every pitch how it is distributed (see 'HoriDirection').
-- The resulting edges (repetitions and passing edges) are represented in a child transition.
data Hori i = HoriOp !(M.Map (Pitch i) HoriDirection) !(Edges i)
  deriving (Eq, Ord)

instance (Notation (Pitch i), Show (Pitch i)) => Show (Hori i) where
  show (HoriOp dist m) = "{" <> L.intercalate "," dists <> "} => " <> show m
   where
    dists = showDist <$> M.toList dist
    showDist (p, to) = showNotation p <> "=>" <> show to

-- | 'Leftmost' specialized to the split, freeze, and horizontalize operations of the grammar.
type PVLeftMost i = Leftmost (Split i) Freeze (Hori i)
