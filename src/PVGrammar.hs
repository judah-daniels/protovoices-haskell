{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
module PVGrammar where

import           Common

import           Musicology.Pitch               ( Interval
                                                , Notation(..)
                                                , Pitch
                                                , SInterval
                                                , SPC
                                                , SPitch
                                                , pc
                                                )

import           Control.DeepSeq                ( NFData )
import           Control.Monad.Identity         ( runIdentity )
import           Data.Aeson                     ( (.:)
                                                , FromJSON
                                                , ToJSON
                                                )
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as Aeson
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as S
import           Data.Hashable                  ( Hashable )
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( catMaybes )
import qualified Data.Text.Lazy.IO             as TL
import           Data.Traversable               ( for )
import           GHC.Generics                   ( Generic )
import qualified Internal.MultiSet             as MS
import qualified Musicology.Core               as Music
import qualified Musicology.Core.Slicing       as Music
import qualified Musicology.MusicXML           as MusicXML

-- element types
-- =============

-- slice type: sets of notes
----------------------------

-- | The content type of 'Slice's.
-- Contains a multiset of pitches, representing the notes in a slice.
newtype Notes n = Notes (MS.MultiSet n)
  deriving (Eq, Ord, Generic, NFData, Hashable)

instance (Notation n) => Show (Notes n) where
  show (Notes ns) =
    "{" <> L.intercalate "," (showNote <$> MS.toOccurList ns) <> "}"
   where
    showNote (p, n) = showNotation p <> mult
      where mult = if n /= 1 then "×" <> show n else ""

instance (Notation n, Eq n, Hashable n) => FromJSON (Notes n) where
  parseJSON = Aeson.withArray "List of Notes" $ \notes -> do
    pitches <- mapM parseJSONNote notes
    pure $ Notes $ MS.fromList pitches

-- | Return the notes or start/stop symbols inside a slice.
-- This is useful to get all objects that an 'Edge' can connect to. 
innerNotes :: StartStop (Notes n) -> [StartStop n]
innerNotes (Inner (Notes n)) = Inner <$> MS.distinctElems n
innerNotes Start             = [Start]
innerNotes Stop              = [Stop]

-- transition type: sets of obligatory edges
--------------------------------------------

-- TODO: could this be improved to forbid start/stop symbols on the wrong side?
-- | A proto-voice edge between two nodes (i.e. notes or start/stop symbols).
type Edge n = (StartStop n, StartStop n)

-- | A proto-voice edge between two notes (excluding start/stop symbols).
type InnerEdge n = (n, n)

-- | The content type of 'Transition's.
-- Contains a multiset of normal (terminal) edges and a multiset of (non-terminal) passing edges.
-- The represented edges are those that are definitely used later on.
-- Edges that are not used are dropped before creating a child transition.
-- A transition that contains passing edges cannot be frozen.
data Edges n = Edges
  { edgesT  :: !(S.HashSet (Edge n))            -- ^ the terminal edges
  , edgesNT :: !(MS.MultiSet (InnerEdge n)) -- ^ the non-terminal edges
  }
  deriving (Eq, Ord, Generic, NFData, Hashable)

instance (Hashable n, Eq n) => Semigroup (Edges n) where
  (Edges aT aNT) <> (Edges bT bNT) = Edges (aT <> bT) (aNT <> bNT)

instance (Hashable n, Eq n) => Monoid (Edges n) where
  mempty = Edges mempty MS.empty

instance (Notation n) => Show (Edges n) where
  show (Edges ts nts) = "{" <> L.intercalate "," (tts <> tnts) <> "}"
   where
    tts  = showT <$> S.toList ts
    tnts = showNT <$> MS.toOccurList nts
    showT (p1, p2) = showNotation p1 <> "-" <> showNotation p2
    showNT ((p1, p2), n) =
      showNotation p1 <> ">" <> showNotation p2 <> "×" <> show n

instance (Eq n, Hashable n, Notation n) => FromJSON (Edges n) where
  parseJSON = Aeson.withObject "Edges" $ \v -> do
    regular <- v .: "regular" >>= mapM parseEdge
    passing <- v .: "passing" >>= mapM parseInnerEdge
    pure $ Edges (S.fromList (regular :: [Edge n]))
                 (MS.fromList (passing :: [InnerEdge n]))

topEdges :: (Hashable n) => Edges n
topEdges = Edges (S.singleton (Start, Stop)) MS.empty

-- operations
-- ==========

-- | Marks different types of ornaments in the derivation.
data DoubleOrnament = FullNeighbor
              | FullRepeat
              | LeftRepeatOfRight
              | RightRepeatOfLeft
              | RootNote
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

data Passing = PassingMid
             | PassingLeft
             | PassingRight
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

data LeftOrnament = LeftNeighbor
                  | LeftRepeat
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

data RightOrnament = RightNeighbor
                   | RightRepeat
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

isRepetitionOnLeft :: DoubleOrnament -> Bool
isRepetitionOnLeft FullRepeat        = True
isRepetitionOnLeft RightRepeatOfLeft = True
isRepetitionOnLeft _                 = False

isRepetitionOnRight :: DoubleOrnament -> Bool
isRepetitionOnRight FullRepeat        = True
isRepetitionOnRight LeftRepeatOfRight = True
isRepetitionOnRight _                 = False

-- | Encodes the decisions made in a split operation.
-- Contains a list of elaborations for every parent edge and note.
-- Each elaboration contains the child pitch, and the corresponding ornament.
-- For every produced edge, a decisions is made whether to keep it or not.
data Split n = SplitOp
  { splitTs   :: !(M.Map (Edge n) [(n, DoubleOrnament)])
    -- ^ Maps every regular edge to a list of ornamentations.
  , splitNTs  :: !(M.Map (InnerEdge n) [(n, Passing)])
    -- ^ Maps every passing edge to a passing tone.
    -- Since every passing edge is elaborated exactly once
    -- but there can be several instances of the same edge in a transition,
    -- the "same" edge can be elaborated with several passing notes,
    -- one for each instance of the edge.
  , fromLeft  :: !(M.Map n [(n, RightOrnament)])
    -- ^ Maps notes from the left parent slice to lists of ornamentations.
  , fromRight :: !(M.Map n [(n, LeftOrnament)])
    -- ^ Maps notes from the right parent slice to lists of ornamentations.
  , keepLeft  :: !(S.HashSet (Edge n))
    -- ^ The set of regular edges to keep in the left child transition.
  , keepRight :: !(S.HashSet (Edge n))
    -- ^ The set of regular edges to keep in the right child transition.
  , passLeft  :: !(MS.MultiSet (InnerEdge n))
    -- ^ Contains the new passing edges introduced in the left child transition
    -- (excluding those passed down from the parent transition).
  , passRight :: !(MS.MultiSet (InnerEdge n))
    -- ^ Contains the new passing edges introduced in the right child transition
    -- (excluding those passed down from the parent transition).
  }
  deriving (Eq, Ord, Generic, NFData)

instance (Notation n) => Show (Split n) where
  show (SplitOp ts nts ls rs kl kr pl pr) =
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
      <> ", pl:"
      <> showOps passLs
      <> ", pr:"
      <> showOps passRs
   where
    showOps ops = "{" <> L.intercalate "," ops <> "}"
    showEdge (n1, n2) = showNotation n1 <> "-" <> showNotation n2
    showChild (p, o) = showNotation p <> ":" <> show o
    showChildren cs = "[" <> L.intercalate "," (showChild <$> cs) <> "]"

    showSplit (e, cs) = showEdge e <> "=>" <> showChildren cs
    showL (p, lchilds) = showNotation p <> "=>" <> showChildren lchilds
    showR (p, rchilds) = showChildren rchilds <> "<=" <> showNotation p

    opTs   = showSplit <$> M.toList ts
    opNTs  = showSplit <$> M.toList nts
    opLs   = showL <$> M.toList ls
    opRs   = showR <$> M.toList rs
    keepLs = showEdge <$> S.toList kl
    keepRs = showEdge <$> S.toList kr
    passLs = showEdge <$> MS.toList pl
    passRs = showEdge <$> MS.toList pr

instance (Ord n, Hashable n) => Semigroup (Split n) where
  (SplitOp ta nta la ra kla kra pla pra) <> (SplitOp tb ntb lb rb klb krb plb prb)
    = SplitOp (ta <+> tb)
              (nta <+> ntb)
              (la <+> lb)
              (ra <+> rb)
              (S.union kla klb)
              (S.union kra krb)
              (MS.union pla plb)
              (MS.union pra prb)
   where
    (<+>) :: (Ord k, Semigroup a) => M.Map k a -> M.Map k a -> M.Map k a
    (<+>) = M.unionWith (<>)

instance (Ord n, Hashable n) => Monoid (Split n) where
  mempty =
    SplitOp M.empty M.empty M.empty M.empty S.empty S.empty MS.empty MS.empty

instance (Notation n, Ord n, Hashable n) => FromJSON (Split n) where
  parseJSON = Aeson.withObject "Split" $ \v -> do
    regular <- v .: "regular" >>= mapM (parseElaboration parseEdge)
    passing <- v .: "passing" >>= mapM (parseElaboration parseInnerEdge)
    fromL   <- v .: "fromLeft" >>= mapM (parseElaboration parseJSONNote)
    fromR   <- v .: "fromRight" >>= mapM (parseElaboration parseJSONNote)
    keepL   <- v .: "keepLeft" >>= mapM parseEdge
    keepR   <- v .: "keepRight" >>= mapM parseEdge
    passL   <- v .: "passLeft" >>= mapM parseInnerEdge
    passR   <- v .: "passRight" >>= mapM parseInnerEdge
    pure $ SplitOp (M.fromList regular)
                   (M.fromList passing)
                   (M.fromList fromL)
                   (M.fromList fromR)
                   (S.fromList keepL)
                   (S.fromList keepR)
                   (MS.fromList (passL :: [InnerEdge n]))
                   (MS.fromList (passR :: [InnerEdge n]))
   where
    parseElaboration
      :: (Notation n, FromJSON o)
      => (Aeson.Value -> Aeson.Parser p)
      -> Aeson.Value
      -> Aeson.Parser (p, [(n, o)])
    parseElaboration parseParent = Aeson.withObject "Elaboration" $ \reg -> do
      parent   <- reg .: "parent" >>= parseParent
      children <- reg .: "children" >>= mapM parseChild
      pure (parent, children)
    parseChild
      :: (Notation n, FromJSON o) => Aeson.Value -> Aeson.Parser (n, o)
    parseChild = Aeson.withObject "Child" $ \cld -> do
      child <- cld .: "child" >>= parseJSONNote
      orn   <- cld .: "orn"
      pure (child, orn)

-- | Represents a freeze operation.
-- Since this just ties all remaining edges
-- (which must all be repetitions)
-- no decisions have to be encoded.
data Freeze = FreezeOp
  deriving (Eq, Ord, Generic, NFData)

instance Show Freeze where
  show _ = "()"

instance FromJSON Freeze where
  parseJSON _ = pure FreezeOp

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
  deriving (Eq, Ord, Show, Generic, NFData)

instance Semigroup HoriDirection where
  ToLeft  l1 <> ToLeft  l2 = ToLeft (l1 + l2)
  ToRight l1 <> ToRight l2 = ToLeft (l1 + l2)
  ToLeft l <> ToRight r | l == r    = ToBoth
                        | l < r     = ToRight (r - l)
                        | otherwise = ToLeft (l - r)
  ToBoth <> other = other
  a      <> b     = b <> a

instance Monoid HoriDirection where
  mempty = ToBoth

-- | Represents a horzontalization operation.
-- Records for every pitch how it is distributed (see 'HoriDirection').
-- The resulting edges (repetitions and passing edges) are represented in a child transition.
data Hori n = HoriOp !(HM.HashMap n HoriDirection) !(Edges n)
  deriving (Eq, Ord, Generic, NFData)

instance (Notation n) => Show (Hori n) where
  show (HoriOp dist m) = "{" <> L.intercalate "," dists <> "} => " <> show m
   where
    dists = showDist <$> HM.toList dist
    showDist (p, to) = showNotation p <> "=>" <> show to

instance (Notation n, Eq n, Hashable n) => FromJSON (Hori n) where
  parseJSON = Aeson.withObject "Hori" $ \v -> do
    dists <- v .: "children" >>= mapM parseDist
    edges <- v .: "midEdges"
    pure $ HoriOp (HM.fromListWith (<>) dists) edges
   where
    parseDist = Aeson.withObject "HoriDist" $ \dst -> do
      parent <- dst .: "parent" >>= parseJSONNote
      child  <- dst .: "child" >>= parseChild
      pure (parent, child)
    parseChild = Aeson.withObject "HoriChild" $ \cld -> do
      typ <- cld .: "type"
      case typ of
        "leftChild"    -> pure $ ToLeft 1
        "rightChild"   -> pure $ ToRight 1
        "bothChildren" -> pure ToBoth
        _              -> Aeson.unexpected typ

-- | 'Leftmost' specialized to the split, freeze, and horizontalize operations of the grammar.
type PVLeftmost n = Leftmost (Split n) Freeze (Hori n)

-- helpers
-- =======

parseJSONNote :: Notation n => Aeson.Value -> Aeson.Parser n
parseJSONNote = Aeson.withObject "Note" $ \v -> do
  pitch <- v .: "pitch"
  case readNotation pitch of
    Just p  -> pure p
    Nothing -> fail $ "Could not parse pitch " <> pitch

parseEdge
  :: Notation n => Aeson.Value -> Aeson.Parser (StartStop n, StartStop n)
parseEdge = Aeson.withObject "Edge" $ \v -> do
  l <- v .: "left" >>= mapM parseJSONNote -- TODO: this might be broken wrt. StartStop
  r <- v .: "right" >>= mapM parseJSONNote
  pure (l, r)

parseInnerEdge :: Notation n => Aeson.Value -> Aeson.Parser (n, n)
parseInnerEdge = Aeson.withObject "InnerEdge" $ \v -> do
  l <- v .: "left"
  r <- v .: "right"
  case (l, r) of
    (Inner il, Inner ir) -> do
      pl <- parseJSONNote il
      pr <- parseJSONNote ir
      pure (pl, pr)
    _ -> fail "Edge is not an inner edge"

type PVAnalysis n = Analysis (Split n) Freeze (Hori n) (Edges n) (Notes n)

loadAnalysis :: FilePath -> IO (Either String (PVAnalysis SPitch))
loadAnalysis = Aeson.eitherDecodeFileStrict

loadAnalysis' :: FilePath -> IO (Either String (PVAnalysis SPC))
loadAnalysis' fn = fmap (analysisMapPitch (pc @SInterval)) <$> loadAnalysis fn

slicesFromFile :: FilePath -> IO [[(SPitch, Music.RightTied)]]
slicesFromFile file = do
  txt <- TL.readFile file
  case MusicXML.parseWithoutIds txt of
    Nothing  -> pure []
    Just doc -> do
      let (xmlNotes, _) = MusicXML.parseScore doc
          notes         = MusicXML.asNoteHeard <$> xmlNotes
          slices        = Music.slicePiece Music.tiedSlicer notes
      pure $ mkSlice <$> filter (not . null) slices
 where
  mkSlice notes = mkNote <$> notes
  mkNote (note, tie) = (Music.pitch note, Music.rightTie tie)

slicesToPath
  :: (Interval i, Ord i, Eq i)
  => [[(Pitch i, Music.RightTied)]]
  -> Path [Pitch i] [Edge (Pitch i)]
slicesToPath = go
 where
  -- normalizeTies (s : next : rest) = (fixTie <$> s)
  --   : normalizeTies (next : rest)
  --  where
  --   nextNotes = fst <$> next
  --   fixTie (p, t) = if p `L.elem` nextNotes then (p, t) else (p, Ends)
  -- normalizeTies [s] = [map (fmap $ const Ends) s]
  -- normalizeTies []  = []
  mkSlice = fmap fst
  mkEdges notes = catMaybes $ mkEdge <$> notes
   where
    mkEdge (_, Music.Ends ) = Nothing
    mkEdge (p, Music.Holds) = Just (Inner p, Inner p)
  go []             = error "cannot construct path from empty list"
  go [notes       ] = PathEnd (mkSlice notes)
  go (notes : rest) = Path (mkSlice notes) (mkEdges notes) $ go rest

loadInput :: FilePath -> IO (Path [Pitch SInterval] [Edge (Pitch SInterval)])
loadInput = fmap slicesToPath . slicesFromFile

loadInput'
  :: FilePath
  -> Int
  -> Int
  -> IO (Path [Pitch SInterval] [Edge (Pitch SInterval)])
loadInput' fn from to =
  slicesToPath . drop (from - 1) . take to <$> slicesFromFile fn


analysisTraversePitches
  :: (Applicative f, Eq n', Hashable n', Ord n')
  => (n -> f n')
  -> PVAnalysis n
  -> f (PVAnalysis n')
analysisTraversePitches f (Analysis deriv top) = do
  deriv' <- traverse (leftmostTraversePitch f) deriv
  top'   <- pathTraversePitch f top
  pure $ Analysis deriv' top'

analysisMapPitch
  :: (Eq n', Hashable n', Ord n') => (n -> n') -> PVAnalysis n -> PVAnalysis n'
analysisMapPitch f = runIdentity . analysisTraversePitches (pure . f)

pathTraversePitch
  :: (Applicative f, Eq n', Hashable n')
  => (n -> f n')
  -> Path (Edges n) (Notes n)
  -> f (Path (Edges n') (Notes n'))
pathTraversePitch f (Path e a rest) = do
  e'    <- edgesTraversePitch f e
  a'    <- notesTraversePitch f a
  rest' <- pathTraversePitch f rest
  pure $ Path e' a' rest'
pathTraversePitch f (PathEnd e) = PathEnd <$> edgesTraversePitch f e

traverseEdge :: Applicative f => (n -> f n') -> (n, n) -> f (n', n')
traverseEdge f (n1, n2) = (,) <$> f n1 <*> f n2

traverseSet
  :: (Applicative f, Eq n', Hashable n')
  => (n -> f n')
  -> S.HashSet n
  -> f (S.HashSet n')
traverseSet f set = S.fromList <$> traverse f (S.toList set)

notesTraversePitch
  :: (Eq n, Hashable n, Applicative f) => (a -> f n) -> Notes a -> f (Notes n)
notesTraversePitch f (Notes notes) = Notes <$> MS.traverse f notes

edgesTraversePitch
  :: (Applicative f, Eq n', Hashable n')
  => (n -> f n')
  -> Edges n
  -> f (Edges n')
edgesTraversePitch f (Edges reg pass) = do
  reg'  <- traverseSet (traverseEdge (traverse f)) reg
  pass' <- MS.traverse (traverseEdge f) pass
  pure $ Edges reg' pass'

leftmostTraversePitch
  :: (Applicative f, Eq n', Hashable n', Ord n')
  => (n -> f n')
  -> Leftmost (Split n) Freeze (Hori n)
  -> f (Leftmost (Split n') Freeze (Hori n'))
leftmostTraversePitch f lm = case lm of
  LMSplitLeft     s  -> LMSplitLeft <$> splitTraversePitch f s
  LMSplitRight    s  -> LMSplitRight <$> splitTraversePitch f s
  LMSplitOnly     s  -> LMSplitOnly <$> splitTraversePitch f s
  LMFreezeLeft    fr -> pure $ LMFreezeLeft fr
  LMFreezeOnly    fr -> pure $ LMFreezeOnly fr
  LMHorizontalize h  -> LMHorizontalize <$> horiTraversePitch f h

splitTraversePitch
  :: forall f n n'
   . (Applicative f, Ord n', Hashable n')
  => (n -> f n')
  -> Split n
  -> f (Split n')
splitTraversePitch f (SplitOp ts nts ls rs kl kr pl pr) = do
  ts'  <- traverseElabo (traverseEdge (traverse f)) ts
  nts' <- traverseElabo (traverseEdge f) nts
  ls'  <- traverseElabo f ls
  rs'  <- traverseElabo f rs
  kl'  <- traverseSet (traverseEdge (traverse f)) kl
  kr'  <- traverseSet (traverseEdge (traverse f)) kr
  pl'  <- MS.traverse (traverseEdge f) pl
  pr'  <- MS.traverse (traverseEdge f) pr
  pure $ SplitOp ts' nts' ls' rs' kl' kr' pl' pr'
 where
  traverseElabo
    :: forall p p' o
     . (Ord p')
    => (p -> f p')
    -> M.Map p [(n, o)]
    -> f (M.Map p' [(n', o)])
  traverseElabo fparent mp = fmap M.fromList $ for (M.toList mp) $ \(e, cs) ->
    do
      e'  <- fparent e
      cs' <- traverse (\(n, o) -> (, o) <$> f n) cs
      pure (e', cs')

horiTraversePitch
  :: (Applicative f, Eq n', Hashable n') => (n -> f n') -> Hori n -> f (Hori n')
horiTraversePitch f (HoriOp dist edges) = do
  dist'  <- traverse (\(k, v) -> (, v) <$> f k) $ HM.toList dist
  edges' <- edgesTraversePitch f edges
  pure $ HoriOp (HM.fromListWith (<>) dist') edges'
