{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveAnyClass #-}
module PVGrammar where

import           Common
import           Display

import           Musicology.Pitch

import qualified Data.HashSet                  as S
-- import qualified Data.Set.Internal             as S
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.Map.Strict.Internal      as M
import qualified Internal.MultiSet             as MS
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
import           GHC.Generics                   ( Generic )
import           Control.DeepSeq                ( NFData )
import           Data.Hashable                  ( Hashable(hashWithSalt) )
import qualified Data.HashMap.Strict           as HM
-- element types
-- =============

-- deriving instance (Generic k, Generic v) => Generic (M.Map k v)
-- instance (Hashable k, Hashable v, Generic k, Generic v) => Hashable (M.Map k v)
-- deriving instance Generic Int

-- deriving instance Generic (S.Set a)
-- instance Hashable a => Hashable (S.Set a)

-- instance (Generic a, Hashable a) => Hashable (MS.MultiSet a) where
--   hashWithSalt i m = hashWithSalt i (MS.toMap m)

-- slice type: sets of notes
----------------------------


-- | The content type of 'Slice's.
-- Contains a multiset of pitches, representing the notes in a slice.
newtype Notes i = Notes (MS.MultiSet (Pitch i))
  deriving (Eq, Ord, Generic, NFData, Hashable)

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
               { edgesT  :: !(S.HashSet (Edge i))            -- ^ the terminal edges
               , edgesNT :: !(MS.MultiSet (InnerEdge i)) -- ^ the non-terminal edges
               }
  deriving (Eq, Ord, Generic, NFData, Hashable)

instance (Notation (Pitch i)) => Show (Edges i) where
  show (Edges ts nts) = "{" <> L.intercalate "," (tts <> tnts) <> "}"
   where
    tts  = showT <$> S.toList ts
    tnts = showNT <$> MS.toOccurList nts
    showT (p1, p2) = showNotation p1 <> "-" <> showNotation p2
    showNT ((p1, p2), n) =
      showNotation p1 <> ">" <> showNotation p2 <> "×" <> show n

-- operations
-- ==========

-- | Marks different types of ornaments in the derivation.
data Ornament = FullNeighbor
              | FullRepeat
              | LeftRepeatOfRight
              | RightRepeatOfLeft
              | RootNote
  deriving (Eq, Ord, Show, Generic)

instance NFData Ornament

data Passing = PassingMid
             | PassingLeft
             | PassingRight
  deriving (Eq, Ord, Show, Generic)

instance NFData Passing

data LeftOrnament = SingleLeftNeighbor
                  | SingleLeftRepeat
  deriving (Eq, Ord, Show, Generic)

instance NFData LeftOrnament

data RightOrnament = SingleRightNeighbor
                   | SingleRightRepeat
  deriving (Eq, Ord, Show, Generic)

instance NFData RightOrnament

isRepetitionOnLeft FullRepeat        = True
isRepetitionOnLeft RightRepeatOfLeft = True
isRepetitionOnLeft _                 = False

isRepetitionOnRight FullRepeat        = True
isRepetitionOnRight LeftRepeatOfRight = True
isRepetitionOnRight _                 = False

-- | Encodes the decisions made in a split operation.
-- Contains a list of elaborations for every parent edge and note.
-- Each elaboration contains the child pitch, and the corresponding ornament.
-- For every produced edge, a decisions is made whether to keep it or not.
data Split i = SplitOp
  { splitTs :: !(M.Map (Edge i) [(Pitch i, Ornament)])
  , splitNTs :: !(M.Map (InnerEdge i) [(Pitch i, Passing)])
  , fromLeft :: !(M.Map (Pitch i) [(Pitch i, RightOrnament)])
  , fromRight :: !(M.Map (Pitch i) [(Pitch i, LeftOrnament)])
  , keepLeft :: !(S.HashSet (Edge i))
  , keepRight :: !(S.HashSet (Edge i))
  }
  deriving (Eq, Ord, Generic)

instance (NFData i) => NFData (Split i)

instance (Notation (Pitch i)) => Show (Split i) where
  show (SplitOp ts nts ls rs kl kr) =
    "ts:"
      <> showOps opTs
      <> ", nts:"
      <> showOps opNTs
      <> ", ls:"
      <> showOps opLs
      <> ", rs:"
      <> showOps opRs
      <> ", kl:"
      <> showOps keepLs
      <> ", kr:"
      <> showOps keepRs
   where
    showOps ops = "{" <> L.intercalate "," ops <> "}"
    showEdge (n1, n2) = showNotation n1 <> "-" <> showNotation n2
    showChild (p, o) = showNotation p <> ":" <> show o
    showChildren cs = "[" <> L.intercalate "," (showChild <$> cs) <> "]"

    showSplit (e, cs) = showEdge e <> "=>" <> showChildren cs
    showL (p, ls) = showNotation p <> "=>" <> showChildren ls
    showR (p, rs) = showChildren rs <> "<=" <> showNotation p

    opTs   = showSplit <$> M.toList ts
    opNTs  = showSplit <$> M.toList nts
    opLs   = showL <$> M.toList ls
    opRs   = showR <$> M.toList rs
    keepLs = showEdge <$> S.toList kl
    keepRs = showEdge <$> S.toList kr

instance (Ord i, Hashable i) => Semigroup (Split i) where
  (SplitOp ta nta la ra kla kra) <> (SplitOp tb ntb lb rb klb krb) = SplitOp
    (ta <+> tb)
    (nta <+> ntb)
    (la <+> lb)
    (ra <+> rb)
    (S.union kla klb)
    (S.union kra krb)
   where
    (<+>) :: (Ord k, Semigroup a) => M.Map k a -> M.Map k a -> M.Map k a
    (<+>) = M.unionWith (<>)

instance (Ord i, Hashable i) => Monoid (Split i) where
  mempty = SplitOp M.empty M.empty M.empty M.empty S.empty S.empty

-- | Represents a freeze operation.
-- Since this just ties all remaining edges
-- (which must all be repetitions)
-- no decisions have to be encoded.
data Freeze = FreezeOp
  deriving (Eq, Ord, Generic)

instance NFData Freeze

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
  deriving (Eq, Ord, Show, Generic)

instance NFData HoriDirection

-- | Represents a horzontalization operation.
-- Records for every pitch how it is distributed (see 'HoriDirection').
-- The resulting edges (repetitions and passing edges) are represented in a child transition.
data Hori i = HoriOp !(HM.HashMap (Pitch i) HoriDirection) !(Edges i)
  deriving (Eq, Ord, Generic)

instance NFData i => NFData (Hori i)

instance (Notation (Pitch i), Show (Pitch i)) => Show (Hori i) where
  show (HoriOp dist m) = "{" <> L.intercalate "," dists <> "} => " <> show m
   where
    dists = showDist <$> HM.toList dist
    showDist (p, to) = showNotation p <> "=>" <> show to

-- | 'Leftmost' specialized to the split, freeze, and horizontalize operations of the grammar.
type PVLeftMost i = Leftmost (Split i) Freeze (Hori i)

