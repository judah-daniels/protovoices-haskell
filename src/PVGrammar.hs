{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module PVGrammar where

import Parser

import           Musicology.Core               as MT

import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.List                      ( intersperse
                                                , nub
                                                )

import qualified Data.Semiring                 as R

import qualified Algebra.Graph.Class           as G
import qualified Algebra.Graph.AdjacencyMap    as GA

import           Control.DeepSeq
import           Control.Monad                  ( foldM )

import           GHC.Generics                   ( Generic )


-- content of slices and transitions (voice graph specific)
-- ========================================================

-- edges (transitions)
----------------------

-- | Used to annotate transitions with additional symbols.
-- Currently distinguishes between inferred and merged transitions.
-- Facilitates restrictions on the derivation order.
data EdgesSymbol = EInf
                 | EMrg
  deriving (Ord, Eq, Show, Generic)

instance NFData EdgesSymbol

-- | A terminal edge between pitches and/or ⋊ and ⋉.
type Edge i = (StartStop (Pitch i), StartStop (Pitch i))

-- | The content of a voice-graph 'Transition'.
-- Sets of terminal and non-terminal edges between pitches,
-- plus a special symbol.
data Edges i = Edges { eTerm :: S.Set (Edge i)
                     , eNonTerm :: S.Set (Pitch i, Pitch i)
                     , eSymbol :: EdgesSymbol
                     }
  deriving (Eq, Ord, Generic)

-- | Returns a terminal 'Edges' object for a given set of edges.
mkEdges :: S.Set (StartStop (Pitch i), StartStop (Pitch i)) -> Edges i
mkEdges es = Edges es S.empty EInf

-- instances for Edges

instance (NFData i) => NFData (Edges i)

instance (Show (Pitch i)) => Show (Edges i) where
  show (Edges t n s) = es
   where
    es = mconcat
      $ intersperse "," ((showT <$> S.toList t) <> (showNT <$> S.toList n))
    showT (f, t) = show f <> "-" <> show t
    showNT (f, t) = show f <> ">" <> show t

instance Ord i => Semigroup (Edges i) where
  (Edges t1 n1 s) <> (Edges t2 n2 _) = Edges (t1 <> t2) (n1 <> n2) s

instance Ord i => Monoid (Edges i) where
  mempty = Edges mempty mempty EInf

-- notes (slices)

-- | Used to annotate slices with additional symbols.
-- Currently distinguishes between terminal and non-terminal slices.
-- Facilitates restrictions on the derivation order.
data NotesSymbol = NTerm
                 | NNonTerm
  deriving (Eq, Ord, Generic)

instance NFData NotesSymbol

instance Show NotesSymbol where
  show NTerm    = "T"
  show NNonTerm = "N"

-- | The content of a voice-graph 'Slice'.
-- Contains a set of pitches and a 'NotesSymbol'
data Notes i = Notes (S.Set (Pitch i)) NotesSymbol
  deriving (Eq, Ord, Generic)

-- | Returns a terminal slice with the given set of notes.
mkNotes :: S.Set (Pitch i) -> Notes i
mkNotes notes = Notes notes NTerm

instance (NFData i) => NFData (Notes i)

instance (Notation (Pitch i)) => Show (Notes i) where
  show (Notes ns symb) =
    "("
      <> mconcat (intersperse "," (showNotation <$> S.toList ns))
      <> ")@"
      <> show symb

-- some shorthands
------------------

-- | A voice-graph 'Transition' (containing 'Edges').
type VTrans i = Transition (Edges i) (Notes i)

-- | A voice-graph 'Slice' (containing 'Notes').
type VSlice i = Slice (Notes i)

-- voice-graph evaluation
-------------------------

-- Helper
-- | Return all notes in a 'VSlice', wrapped in a 'StartStop'.
allNodes :: (Ord i) => VSlice i -> S.Set (StartStop (Pitch i))
allNodes (Slice _ _ (Inner (Notes ns _))) = S.map Inner ns
allNodes (Slice _ _ (:⋊)                ) = S.singleton (:⋊)
allNodes (Slice _ _ (:⋉)                ) = S.singleton (:⋉)

-- | Either a terminal or a non-terminal edge.
type EdgeOrNtEdge i = Either (Edge i) (Pitch i, Pitch i)

-- | A reduction rule for a voice-leading elaboration.
-- Takes a pair of terminal and/or non-terminal edges.
-- If the edges fit the rule, returns 'Just' the resulting (terminal or non-terminal) edge.
-- Otherwise, returns 'Nothing'.
type VRule i
  =  EdgeOrNtEdge i
  -> EdgeOrNtEdge i
  -> Maybe (Either (Edge i) (Pitch i, Pitch i))

-- TODO: this might need to include dropped (unelaborated) edges
-- For each note in the slice list all possible ways to reduce it
-- by considering all pair of incoming and outgoing edges and compairing them to a rule
-- Each reduction produces a new edge across the slice.
-- Return all combinations of reducting each note (i.e. all combinations of reduction edges)
-- | Given a list of reduction rules, a pair of transitions and a set of notes,
-- returns a list of possible merges.
-- Used to construct a merge evaluator for voice graphs.
parseSplit
  :: forall i
   . (Ord i)
  => [VRule i]
  -> Edges i
  -> StartStop (Notes i)
  -> Edges i
  -> [Edges i]
parseSplit rules t1 (Inner (Notes notes _)) t2 = S.toList parses
 where
  -- The product of all reductions is the set of all combinations of reductions
  -- i.e. r1 x r2 x ... rn where r1 is the set of reductions for note i.
  -- The product is similar to the cartesian product but mappends all selected Edges.
  -- Πᵢ₌₁ⁿ rᵢ = { e₁ <> ... <> eₙ | eᵢ ∈ rᵢ }
  -- Since each Edges object is a singleton, mappening them yields a full transition
  -- with one edge per reduced note.
  parses = R.product reductions
  reductions :: S.Set (S.Set (Edges i))
  reductions = S.map noteReductions notes
  noteReductions :: Pitch i -> S.Set (Edges i)
  noteReductions n = foldMap applyRules (edgePairs n)

  -- all pairs of incoming edges to and outgoing edges from a note n
  edgePairs :: Pitch i -> S.Set (EdgeOrNtEdge i, EdgeOrNtEdge i)
  edgePairs n = foldMap (\i -> S.map (i, ) out) (incoming n)
    where out = outgoing n
  -- incoming terminal and non-terminal edges into n
  incoming :: Pitch i -> S.Set (EdgeOrNtEdge i)
  incoming n =
    S.map Left (S.filter (\e -> snd e == Inner n) (eTerm t1))
      <> S.map Right (S.filter (\e -> snd e == n) (eNonTerm t1))
  -- outgoing terminal and non-terminal edges from n
  outgoing :: Pitch i -> S.Set (EdgeOrNtEdge i)
  outgoing n =
    S.map Left (S.filter (\e -> fst e == Inner n) (eTerm t2))
      <> S.map Right (S.filter (\e -> fst e == n) (eNonTerm t2))

  -- apply all rules and concatenate the resulting reductions
  applyRules :: (EdgeOrNtEdge i, EdgeOrNtEdge i) -> S.Set (Edges i)
  applyRules edgePair = foldMap (applyRule edgePair) rules
  -- returns singleton Edges, containing just one reduction edge
  applyRule :: (EdgeOrNtEdge i, EdgeOrNtEdge i) -> VRule i -> S.Set (Edges i)
  applyRule (e1, e2) rule = case rule e1 e2 of
    Just (Left  t ) -> S.singleton $ Edges (S.singleton t) S.empty EMrg
    Just (Right nt) -> S.singleton $ Edges S.empty (S.singleton nt) EMrg
    Nothing         -> S.empty

parseSplit _ _ _ _ = [] -- not an inner node? cannot merge

-- just take the union of the two slices, under two conditions:
-- 1. duplicate notes must have an edge between them
-- 2. all other pairs of notes must not have a terminal edge
-- non-terminal edges can be ignored here
-- | Given a pair of note sets and a set of edges in between,
-- returns all possible verticalizations of the note sets.
-- Used to construct a verticalization evaluator for voice graphs.
parseHoris
  :: (Ord i)
  => StartStop (Notes i)
  -> Edges i
  -> StartStop (Notes i)
  -> [Notes i]
-- if the left slice is a terminal, vert is always allowed
parseHoris (Inner (Notes s1 NTerm)) (Edges edges _ _) (Inner (Notes s2 _))
  | vertable s1 s2 edges = [Notes (S.union s1 s2) NNonTerm]
-- if the left slice is a non-terminal, the transition must be non-terminal too.
parseHoris (Inner (Notes s1 NNonTerm)) (Edges edges _ EMrg) (Inner (Notes s2 _))
  | vertable s1 s2 edges
  = [Notes (S.union s1 s2) NNonTerm]
parseHoris _ _ _ = [] -- fail otherwise

-- helper
-- | Tests whether two note sets can be verticalized using a set of edges.
vertable s1 s2 edges =
  all (\p -> (Inner p, Inner p) `S.member` edges) dups -- cond 1
    && all (uncurry (==)) edges               -- cond 2
  where dups = S.intersection s1 s2

-- suggest:
-- 1. only repeating edges
-- 2. all combinations of edges such that the left slice is covered
-- 3. all combinations of edges such that the right slice is covered
-- | Returns a list of possible transitions for two given slices.
suggestEdges
  :: (Diatonic i, Ord i)
  => StartStop (Notes i)
  -> StartStop (Notes i)
  -> [Edges i]
suggestEdges (:⋊) (:⋉) = [mkEdges $ S.singleton ((:⋊), (:⋉))]
suggestEdges (:⋊) (Inner (Notes notes _)) =
  [mkEdges $ S.map (\n -> ((:⋊), Inner n)) notes]
suggestEdges (Inner (Notes notes _)) (:⋉) =
  [mkEdges $ S.map (\n -> (Inner n, (:⋉))) notes]
suggestEdges (Inner (Notes lns _)) (Inner (Notes rns _)) =
  fmap mkEdges $ nub $ inter <> left <> right
 where
  lnotes = S.toList lns
  rnotes = S.toList rns
  -- just connect duplicate notes
  inter  = [S.map (\n -> (Inner n, Inner n)) $ S.intersection lns rns]
  -- connect all left notes with some right note
  left   = foldM addRight S.empty lnotes
  addRight es ln = do
    rn <- filter (\n -> isStep $ ln `pto` n) rnotes
    return $ S.insert (Inner ln, Inner rn) es
  -- connect all right notes with some left note
  right = foldM addLeft S.empty rnotes
  addLeft es rn = do
    ln <- filter (\n -> isStep $ n `pto` rn) lnotes
    return $ S.insert (Inner ln, Inner rn) es

-- | Creates a voice-graph evaluator from a set of evaluation functions
-- and semiring mappings.
mkVoiceEvaluator
  :: (Edges i -> StartStop (Notes i) -> Edges i -> [Edges i])
  -> (StartStop (Notes i) -> Edges i -> StartStop (Notes i) -> [Notes i])
  -> (StartStop (Notes i) -> StartStop (Notes i) -> [Edges i])
  -> (VTrans i -> VTrans i -> VSlice i -> VTrans i -> v)
  -> (VSlice i -> VSlice i -> VTrans i -> VSlice i -> v)
  -> (VTrans i -> VSlice i -> VSlice i -> v)
  -> (VSlice i -> v)
  -> Eval (Edges i) (Notes i) v
mkVoiceEvaluator parseSplit parseHoris suggestEdges fromSplit fromHori fromEdges fromSlice
  = Eval vert inf merge fromSlice
 where
  merge t1 s t2 = mkResults (\r -> fromSplit r t1 s t2) results
   where
    wrap e = Transition (tLeftSlice t1) (tRightSlice t2) Nothing Nothing e -- wrap edges into a transition
    results = wrap <$> parseSplit (tEdges t1) (sContent s) (tEdges t2)
  vert s1 t s2 = mkResults (\r -> fromHori r s1 t s2) results
   where
    wrap    = Slice (sFirst s1) (sLast s2) . Inner -- wrap notes into a slice
    results = wrap <$> parseHoris (sContent s1) (tEdges t) (sContent s2)
  inf s1 s2 = mkResults (\r -> fromEdges r s1 s2) results
   where
    -- TODO: this is incorrect (verticalized transitions should have correct dependencies)
    -- but fixing it requires restructuring the whole parser
    wrap e = Transition s1 s2 Nothing Nothing e -- wrap edges into a transition
    results = wrap <$> suggestEdges (sContent s1) (sContent s2)
  mkResults f results = fmap (\r -> (r, f r)) results

-- default VL elaboration rules
-------------------------------

neighbor :: (Diatonic i, Eq i) => VRule i
neighbor (Left (Inner pl, Inner pm)) (Left (_, Inner pr))
  | pl == pr && pm /= pl = Just $ Left (Inner pl, Inner pr)
neighbor _ _ = Nothing

startNeighbor :: (Diatonic i) => VRule i
startNeighbor (Left ((:⋊), Inner pm)) (Left (_, Inner pr)) =
  Just $ Left ((:⋊), Inner pr)
startNeighbor _ _ = Nothing

between pl pm pr =
  pl /= pm && pm /= pr && pl /= pr && dir1 == odir && dir2 == odir
 where
  odir = direction $ pl `pto` pr
  dir1 = direction $ pl `pto` pm
  dir2 = direction $ pm `pto` pr

passing :: (Interval i, Eq i) => VRule i
passing (Left (Inner pl, Inner pm)) (Left (_, Inner pr)) | between pl pm pr =
  Just $ Right (pl, pr)
passing (Right (pl, pm)) (Left (_, Inner pr)) | between pl pm pr =
  Just $ Right (pl, pr)
passing _ _ = Nothing

dupRight :: Eq i => VRule i
dupRight (Left (Inner pl, Inner pm)) (Left (_, Inner pr)) | pm == pr =
  Just $ Left (Inner pl, Inner pr)
dupRight _ _ = Nothing

dupLeft :: Eq i => VRule i
dupLeft (Left (Inner pl, Inner pm)) (Left (_, Inner pr)) | pl == pm =
  Just $ Left (Inner pl, Inner pr)
dupLeft _ _ = Nothing

top :: VRule i
top (Left ((:⋊), _)) (Left (_, (:⋉))) = Just $ Left ((:⋊), (:⋉))
top _                _                = Nothing

-- | The default list of VL elaboration rules.
defRules :: (Diatonic i, Eq i) => [VRule i]
defRules = [neighbor, passing, dupLeft, dupRight, top]

-- default stuff
----------------

-- | Returns a voice-graph evaluator from a set of semiring mappings
-- using the default evaluation functions.
mkDefVoiceEvaluator
  :: (Diatonic i, Ord i)
  => (VTrans i -> VTrans i -> VSlice i -> VTrans i -> v)
  -> (VSlice i -> VSlice i -> VTrans i -> VSlice i -> v)
  -> (VTrans i -> VSlice i -> VSlice i -> v)
  -> (VSlice i -> v)
  -> Eval (Edges i) (Notes i) v
mkDefVoiceEvaluator =
  mkVoiceEvaluator (parseSplit defRules) parseHoris suggestEdges

-- | Evaluator in the boolean semiring
voiceEvalBool :: (Diatonic i, Ord i) => Eval (Edges i) (Notes i) Bool
voiceEvalBool = mkDefVoiceEvaluator split hori edges slice
 where
  split a b c d = True
  hori a b c d = True
  edges a b c = True
  slice a = True

-- | Evaluator in the counting semiring
voiceEvalCount :: (Diatonic i, Ord i) => Eval (Edges i) (Notes i) Int
voiceEvalCount = mkDefVoiceEvaluator split hori edges slice
 where
  split a b c d = 1
  hori a b c d = 1
  edges a b c = 1
  slice a = 1

-- | parses the input in the boolean semiring
isMusic
  :: ( Show i
     , Diatonic i
     , Eq i
     , Ord i
     , Notation (Pitch i)
     , Show (Pitch i)
     , NFData i
     )
  => [Notes i]
  -> Bool
isMusic = parse voiceEvalBool

-- | parses the input in the counting semiring
countDerivations
  :: ( Show i
     , Diatonic i
     , Ord i
     , Eq i
     , Notation (Pitch i)
     , Show (Pitch i)
     , NFData i
     )
  => [Notes i]
  -> Int
countDerivations = parse voiceEvalCount





-- semiring stuff
-----------------

-- | An evaluator in the derivation semiring.
voiceEvalDerivs
  :: (Diatonic i, Ord i)
  => Eval (Edges i) (Notes i) (Derivations (Edges i) (Notes i))
voiceEvalDerivs = mkDefVoiceEvaluator split hori edges slice
 where
  split r t1 s t2 = S.singleton [Split r t1 s t2]
  hori r s1 t s2 = S.singleton [Horizontalize r s1 t s2]
  edges r s1 s2 = R.one -- S.singleton [EmitTrans r]
  slice r = S.singleton [EmitSlice r]

-- | Parse a piece in the derivation semiring.
allDerivations
  :: ( Show i
     , Diatonic i
     , Ord i
     , Eq i
     , Notation (Pitch i)
     , Show (Pitch i)
     , NFData i
     )
  => [Notes i]
  -> Derivations (Edges i) (Notes i)
allDerivations = parse voiceEvalDerivs



-- the proto-voice graph semiring
---------------------------------

-- | A note that stands for an a note or ⋊ or ⋉ somewhere in the derivation.
-- The node is indexed by the timeslots it spans on the surface.
data PVNode i = PVNode { pvnPitch :: StartStop (Pitch i)
                       , pvnFirst :: Int
                       , pvnLast  :: Int
                       }
  deriving (Eq, Ord, Generic)

instance (NFData i) => NFData (PVNode i)

deriving instance (Show (Pitch i)) => Show (PVNode i)

-- | A mapping from the notes in a non-terminal slice to their lower-level counterparts.
-- Tracks the leftmost and the rightmost occurrence of each note
-- in the horizontalization of a slice.
data PVSlice i = PVSlice { pvsLeftmost :: M.Map (StartStop (Pitch i)) (Int, Int)
                         , pvsRightmost :: M.Map (StartStop (Pitch i)) (Int, Int)
                         }
  deriving (Eq, Ord, Generic)

instance (NFData i) => NFData (PVSlice i)

deriving instance (Show (Pitch i)) => Show (PVSlice i)

-- | A proto-voice graph that can be used to represent a derivation.
-- In addition to terminal and non-terminal edges,
-- contains 'PVSlice's for all slices occurring in its derivation,
-- in order to map notes in reductions to their surface occurrences.
data PVGraph i = PVGraph { pvgGraph :: GA.AdjacencyMap (PVNode i)
                         , pvgNTGraph :: GA.AdjacencyMap (PVNode i)
                         , pvgSlices :: M.Map (Int,Int) (PVSlice i)
                         }
  deriving (Eq, Ord, Generic)

instance (NFData i) => NFData (PVGraph i)

instance (Show (Pitch i)) => Show (PVGraph i) where
  show (PVGraph g ng s) =
    "Graph:\n"
      <> showFoldable (GA.edgeList g)
      <> "Non-Terminal Graph:\n"
      <> showFoldable (GA.edgeList ng)
      <> "Slices:\n"
      <> show s

-- deriving instance (Show (Pitch i), Ord i) => Show (PVGraph i)

-- | The 'Semigroup' instance of 'PVGraph'.
-- Describes how two proto-voice graphs are combined.
instance (Ord i) => Semigroup (PVGraph i) where
  (PVGraph g1 ng1 s1) <> (PVGraph g2 ng2 s2) = PVGraph (G.overlay g1' g2')
                                                       (G.overlay ng1' ng2')
                                                       (M.union s1' s2')
   where
    s1' = subSlices s1 s2
    s2' = subSlices s2 s1
    subSlices sa sb = M.map
      (\(PVSlice l r) -> PVSlice (subMap l pvsLeftmost) (subMap r pvsRightmost))
      sa
     where
      lookupSlice f t p side =
        M.findWithDefault (f, t) p $ side $ M.findWithDefault
          (PVSlice M.empty M.empty)
          (f, t)
          sb
      subMap m side = M.mapWithKey (\p (f, t) -> lookupSlice f t p side) m
    g1'  = subG g1 s2
    g2'  = subG g2 s1
    ng1' = subG ng1 s2
    ng2' = subG ng2 s1
    subG g slices = GA.edges $ subEdge <$> GA.edgeList g
     where
      subEdge (n1, n2) = (replace n1 pvsRightmost, replace n2 pvsLeftmost)
       where
        replace n1@(PVNode p f t) side = maybe n1 id $ do
          slice    <- M.lookup (f, t) slices
          (f', t') <- M.lookup p $ side slice
          pure $ PVNode p f' t'

-- | The 'Monoid' instance of 'PVGraph'.
-- Adds the empty graph.
instance (Ord i) => Monoid (PVGraph i) where
  mempty = PVGraph G.empty G.empty mempty

-- again use the Semiring instance of set
-- | A set of proto-voice graphs.
-- Forms a 'Semiring' that is analogous to the derivation semiring.
type PVGraphs i = S.Set (PVGraph i)

-- | Returns the proto-voice graph corresponding to a slice.
pvSliceToNodes (Slice f t (Inner (Notes ps _))) =
  fmap (\p -> PVNode (Inner p) f t) (S.toList ps)
pvSliceToNodes (Slice f t (:⋊)) = [PVNode (:⋊) f t]
pvSliceToNodes (Slice f t (:⋉)) = [PVNode (:⋉) f t]

-- | Returns the proto-voice graph corresponding to a transition.
pvTransToEdges (Transition (Slice f1 t1 _) (Slice f2 t2 _) _ _ (Edges t nt _))
  = (mkEdge <$> S.toList t, mkNtEdge <$> S.toList nt)
 where
  mkEdge (n1, n2) = (PVNode n1 f1 t1, PVNode n2 f2 t2)
  mkNtEdge (n1, n2) = (PVNode (Inner n1) f1 t1, PVNode (Inner n2) f2 t2)

-- | An evaluator in the semiring of proto-voice graphs.
voiceEvalGraphs :: (Diatonic i, Ord i) => Eval (Edges i) (Notes i) (PVGraphs i)
voiceEvalGraphs = mkDefVoiceEvaluator split hori trans slice
 where
  split r t1 s t2 = S.singleton $ PVGraph (G.edges te) (G.edges nte) M.empty
    where (te, nte) = pvTransToEdges r
  hori r s1 t s2 = S.singleton $ PVGraph G.empty G.empty slices
   where
    slices = M.singleton (sFirst r, sLast r) $ PVSlice left right
    n1     = allNodes s1
    n2     = allNodes s2
    left   = M.fromSet (\n -> (sFirst s1, sLast s1)) n1
      `M.union` M.fromSet (\n -> (sFirst s2, sLast s2)) (n2 S.\\ n1)
    right = M.fromSet (\n -> (sFirst s2, sLast s2)) n2
      `M.union` M.fromSet (\n -> (sFirst s1, sLast s1)) (n1 S.\\ n2)
  trans r s1 s2 = S.singleton $ PVGraph (G.edges te) (G.edges nte) M.empty
    where (te, nte) = pvTransToEdges r
  slice r =
    S.singleton $ PVGraph (G.vertices $ pvSliceToNodes r) G.empty M.empty

-- | Parses a piece in the semiring of proto-voice graphs.
allGraphs
  :: ( Show i
     , Diatonic i
     , Ord i
     , Eq i
     , Notation (Pitch i)
     , Show (Pitch i)
     , NFData i
     )
  => [Notes i]
  -> PVGraphs i
allGraphs = parse voiceEvalGraphs

-- | Sums the proto-voice graphs that belong to a single piece.
-- Returns the union graph together with a map
-- indicating the number of occurrences of each edge in the original graphs.
sumGraphs
  :: (Foldable t, Ord i)
  => t (PVGraph i)
  -> (PVGraph i, M.Map (PVNode i, PVNode i) Int)
sumGraphs graphs = foldl add (mempty, M.empty) graphs
 where
  add (PVGraph gacc ngacc _, macc) (PVGraph g ng _) =
    (PVGraph (G.overlay gacc g) (G.overlay ngacc ng) M.empty, macc')
   where
    edges = S.union (GA.edgeSet g) (GA.edgeSet ng)
    macc' = M.unionWith (+) macc (M.fromSet (const 1) edges)
