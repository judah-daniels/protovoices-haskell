{-# LANGUAGE ScopedTypeVariables #-}

module HeuristicParser where

import Common
import Data.Maybe

newtype Slice ns = Slice
  { sContent :: ns
  }
  deriving (Eq)

instance Show ns => Show (Slice ns) where
  show (Slice ns) = show ns

data Trans es = Trans
  { tContent :: !es
  , t2nd :: !Bool
  , tBoundary :: !Bool
  }
  deriving (Eq, Show)

{- | The state of the search between steps.
 Generally, the current reduction consists of frozen transitions
 between the ⋊ and the current location
 and open transitions between the current location and ⋉.

 > ⋊==[1]==[2]==[3]——[4]——[5]——⋉
 >   └ frozen  ┘  | └   open  ┘
 >             midSlice (current position)
 >
 > frozen:   ==[2]==[1]==
 > midSlice: [3]
 > open:     ——[4]——[5]——

 This is the 'SSSemiOpen' case:
 The slice at the current pointer (@[3]@)
 is represented as an individual slice (@_ssMidSlice@).
 The frozen part is represented by a 'Path' of frozen transitions (@tr'@) and slices (@slc@).
 __in reverse direction__, i.e. from @midslice@ back to ⋊ (excluding ⋊).
 The open part is a 'Path' of open transitions (@tr@) and slices (@slc@)
 in forward direction from @midSlice@ up to ⋉.

 There are two special cases.
 All transitions can be frozen ('SSFrozen'),
 in which case state only contains the backward 'Path' of frozen transitions
 (excluding ⋊ and ⋉):

 > ⋊==[1]==[2]==[3]==⋉
 >                    └ current position
 > represented as: ==[3]==[2]==[1]==

 Or all transitions can be open ('SSOpen'),
 in which case the state is just the forward path of open transitions:

 > ⋊——[1]——[2]——[3]——⋉
 > └ current position
 > represented as: ——[1]——[2]——[3]——

 The open and semiopen case additionally have a list of operations in generative order.

Types:
es' : Unparsed/frozen transitions
es  : Unfrozen transitions
ns  : Evaluated slice content
o   : Operation type
-}

-- | The state of the search between steps
data SearchState es es' ns o
  = SSFrozen !(Path (Maybe es', Bool) (Slice ns)) -- Beginning of search - all frozen edges and slices
  | SSSemiOpen -- Point between beginning and end of the path
      { _ssFrozen :: !(Path (Maybe es', Bool) (Slice ns))
      -- ^ frozen transitions and slices from current point leftward
      , _ssMidSlice :: !(Slice ns)
      -- ^ the slice at the current posision between gsFrozen and gsOpen
      , _ssOpen :: !(Path (Trans es) (Slice ns))
      -- ^ non-frozen transitions and slices from current point rightward
      , _ssDeriv :: ![o]
      -- ^ derivation from current reduction to original surface
      }
  | SSOpen !(Path (Trans es) (Slice ns)) ![o] -- Single path with unfrozen transition,slices and history

instance (Show ns, Show o) => Show (SearchState es es' ns o) where
  show (SSFrozen frozen) = showFrozen frozen <> "⋉"
  show (SSOpen open ops) = "⋊" <> showOpen open
  show (SSSemiOpen frozen midSlice open ops) = showFrozen frozen <> show midSlice <> showOpen open

-- | Helper function for showing the frozen part of a piece.
showFrozen :: Show slc => Path (Maybe es', Bool) slc -> String
showFrozen path = "⋊" <> go path
 where
  go (PathEnd (_, True)) = "≠"
  go (PathEnd (_, False)) = "="
  go (Path (_, True) a rst) = go rst <> show a <> "≠"
  go (Path (_, False) a rst) = go rst <> show a <> "="

-- | Helper function for showing the open part of a piece.
showOpen :: Show slc => Path (Trans es') slc -> String
showOpen path = go path <> "⋉"
 where
  go (PathEnd (Trans _ _ True)) = "⌿"
  go (PathEnd _) = "-"
  go (Path (Trans _ _ True) a rst) = "⌿" <> show a <> go rst
  go (Path _ a rst) = "-" <> show a <> go rst

{-
 > freeze left:         split left:          split right:         spread:
 > ...=[]——[]——[]—...   ...=[]——[]——[]—...   ...=[]——[]——[]—...   ...=[]——[]——[]—...
 > ...=[]==[]——[]—...        \  /                     \  /             \  /\  /
 >                            []                       []               []——[]
-}

-- | Returns a list of possible next states given the current state.
exploreStates
  :: forall es es' ns ns' o
   . Eval es es' ns ns' o
  -> SearchState es es' ns o
  -> [SearchState es es' ns o]
exploreStates eval state = case state of
  SSFrozen path -> case path of
    PathEnd (t, boundary) -> map genState thawed
     where
      thawed = evalUnfreeze eval Start t Stop True
      genState (t, o) = SSOpen (PathEnd (Trans t False boundary)) [o]
    Path (t, boundary) slice rest -> map genState thawed
     where
      thawed = evalUnfreeze eval Start t (Inner $ sContent slice) False
      genState (t, o) = SSSemiOpen rest slice (PathEnd (Trans t False boundary)) [o]
  SSSemiOpen frozen midSlice open ops -> thawOps <> reductions -- we can either unfreeze, or apply an operation to the open part
   where
    thawOps = case frozen of
      PathEnd (t, boundary) -> map genState thawed
       where
        thawed = evalUnfreeze eval Start t Stop False
        genState (t, op) = SSOpen (Path (Trans t False boundary) midSlice open) (op : ops)
      Path (t, boundary) frozenSlice rest -> map genState thawed
       where
        thawed = evalUnfreeze eval Start t Stop False
        genState (t, op) = SSSemiOpen rest frozenSlice (Path (Trans t False boundary) midSlice open) (op : ops)

    reductions :: [SearchState es es' ns o]
    reductions = case open of
      -- A single transition - No operations
      PathEnd _ -> []
      -- Two open transitions: merge
      Path (Trans tl tl2nd tlBoundary) (Slice slice) (PathEnd (Trans tr _ trBoundary)) -> if (tlBoundary && trBoundary) then [] else map genState merged
       where
        merged = evalUnsplit eval (Inner $ sContent midSlice) tl slice tr Stop SingleOfOne
        genState (parent, op) = SSSemiOpen frozen midSlice (PathEnd (Trans parent tl2nd (tlBoundary || trBoundary))) (op : ops)

      -- Three open transitions: mergeleft mergeRight or verticalise
      Path (Trans tl tl2nd tlBoundary) (Slice sl) (Path (Trans tm tm2nd tmBoundary) (Slice sm) (PathEnd (Trans tr tr2nd trBoundary))) -> leftMergeStates <> rightMergeStates <> vertStates
       where
        -- TODO consider 2nd and boundary
        leftMergeStates = if (tlBoundary && tmBoundary) then [] else map genState leftMerges
         where
          leftMerges = evalUnsplit eval (Inner $ sContent midSlice) tl sl tm (Inner sm) LeftOfTwo
          genState (parent, op) = SSSemiOpen frozen midSlice (Path (Trans parent tl2nd (tlBoundary || tmBoundary)) (Slice sm) (PathEnd (Trans tr tr2nd trBoundary))) (op : ops)

        -- TODO consider 2nd and boundary
        rightMergeStates = if not tm2nd || (tmBoundary && trBoundary) then [] else map genState rightMerges
         where
          rightMerges = evalUnsplit eval (Inner sl) tm sm tr Stop RightOfTwo
          genState (parent, op) = SSSemiOpen frozen midSlice (Path (Trans tl tl2nd tlBoundary) (Slice sl) (PathEnd (Trans parent True (tmBoundary || trBoundary)))) (op : ops)

        vertStates =
          if tl2nd || tmBoundary
            then []
            else do
              -- List
              (sTop, op) <- maybeToList $ evalUnspreadMiddle eval (sl, tm, sm)
              lTop <- evalUnspreadLeft eval (tl, sl) sTop -- Check boundaries TODO
              rTop <- evalUnspreadRight eval (sm, tr) sTop -- TODO boundaris
              pure $ getState lTop sTop rTop op
         where
          getState lTop sTop rTop op = SSSemiOpen frozen midSlice (Path (Trans lTop False False) (Slice sTop) (PathEnd (Trans rTop False False))) (op : ops)
      Path (Trans tl tl2nd tlBoundary) (Slice sl) (Path (Trans tm tm2nd tmBoundary) (Slice sm) (Path (Trans tr tr2nd trBoundary) sr rst)) -> leftMergeStates <> rightMergeStates <> vertStates
       where
        -- TODO consider 2nd and boundary
        leftMergeStates = if (tmBoundary && tlBoundary) then [] else map genState leftMerges
         where
          leftMerges = evalUnsplit eval (Inner $ sContent midSlice) tl sl tm (Inner sm) LeftOfTwo
          genState (parent, op) = SSSemiOpen frozen midSlice (Path (Trans parent tl2nd (tlBoundary || tmBoundary)) (Slice sm) (PathEnd (Trans tr tr2nd trBoundary))) (op : ops)

        -- TODO consider 2nd and boundary
        rightMergeStates = if not tm2nd || (tmBoundary && trBoundary) then [] else map genState rightMerges
         where
          rightMerges = evalUnsplit eval (Inner sl) tl sl tm (Inner sm) RightOfTwo
          genState (parent, op) = SSSemiOpen frozen midSlice (Path (Trans tl tl2nd tlBoundary) (Slice sl) (PathEnd (Trans parent True (tmBoundary || trBoundary)))) (op : ops)

        vertStates =
          if tl2nd || tmBoundary
            then []
            else do
              -- List
              (sTop, op) <- maybeToList $ evalUnspreadMiddle eval (sl, tm, sm)
              lTop <- evalUnspreadLeft eval (tl, sl) sTop -- Check boundaries TODO
              rTop <- evalUnspreadRight eval (sm, tr) sTop -- TODO boundaris
              pure $ getState lTop sTop rTop op
         where
          getState lTop sTop rTop op = SSSemiOpen frozen midSlice (Path (Trans lTop False tlBoundary) (Slice sTop) (Path (Trans rTop tr2nd trBoundary) sr rst)) (op : ops)
  SSOpen open ops -> reductions -- we can either unfreeze, or apply an operation to the open part
   where
    reductions :: [SearchState es es' ns o]
    reductions = case open of
      -- A single transition - No operations
      PathEnd _ -> []
      -- Two open transitions: merge
      Path (Trans tl tl2nd tlBoundary) (Slice slice) (PathEnd (Trans tr _ trBoundary)) -> if (tlBoundary && trBoundary) then [] else map genState merged
       where
        merged = evalUnsplit eval Start tl slice tr Stop SingleOfOne -- Start as left slice correct?
        genState (parent, op) = SSOpen (PathEnd (Trans parent tl2nd (tlBoundary && trBoundary))) (op : ops)

      -- Three open transitions: mergeleft mergeRight or verticalise
      Path (Trans tl tl2nd tlBoundary) (Slice sl) (Path (Trans tm tm2nd tmBoundary) (Slice sm) (PathEnd (Trans tr tr2nd trBoundary))) -> leftMergeStates <> rightMergeStates <> vertStates
       where
        -- TODO consider 2nd and boundary
        leftMergeStates = if (tmBoundary && tlBoundary) then [] else map genState leftMerges
         where
          leftMerges = evalUnsplit eval Start tl sl tm (Inner sm) LeftOfTwo
          genState (parent, op) = SSOpen (Path (Trans parent tl2nd (tlBoundary || tmBoundary)) (Slice sm) (PathEnd (Trans tr tr2nd trBoundary))) (op : ops)

        -- TODO consider 2nd and boundary
        rightMergeStates = if not tm2nd || (tmBoundary && trBoundary) then [] else map genState rightMerges
         where
          rightMerges = evalUnsplit eval (Inner sl) tl sl tm (Inner sm) RightOfTwo
          genState (parent, op) = SSOpen (Path (Trans tl tl2nd tlBoundary) (Slice sl) (PathEnd (Trans parent True (tmBoundary || trBoundary)))) (op : ops)

        vertStates =
          if tl2nd || tmBoundary
            then []
            else do
              -- List
              (sTop, op) <- maybeToList $ evalUnspreadMiddle eval (sl, tm, sm)
              lTop <- evalUnspreadLeft eval (tl, sl) sTop -- Check boundaries TODO
              rTop <- evalUnspreadRight eval (sm, tr) sTop -- TODO boundaris
              pure $ getState lTop sTop rTop op
         where
          getState lTop sTop rTop op = SSOpen (Path (Trans lTop False tlBoundary) (Slice sTop) (PathEnd (Trans rTop False trBoundary))) (op : ops)
      Path (Trans tl tl2nd tlBoundary) (Slice sl) (Path (Trans tm tm2nd tmBoundary) (Slice sm) (Path (Trans tr tr2nd trBoundary) sr rst)) -> leftMergeStates <> rightMergeStates <> vertStates
       where
        -- TODO consider 2nd and boundary
        leftMergeStates = if (not tm2nd) || (tmBoundary && tlBoundary) then [] else map genState leftMerges
         where
          leftMerges = evalUnsplit eval Start tl sl tm (Inner sm) LeftOfTwo
          genState (parent, op) = SSOpen (Path (Trans parent tl2nd (tlBoundary || tmBoundary)) (Slice sm) (PathEnd (Trans tr tr2nd trBoundary))) (op : ops)

        -- TODO consider 2nd and boundary
        rightMergeStates = if (not tm2nd) || (tmBoundary && trBoundary) then [] else map genState rightMerges
         where
          rightMerges = evalUnsplit eval (Inner sl) tl sl tm (Inner sm) RightOfTwo
          genState (parent, op) = SSOpen (Path (Trans tl tl2nd tlBoundary) (Slice sl) (PathEnd (Trans parent True (trBoundary || tmBoundary)))) (op : ops)

        vertStates =
          if tl2nd || tmBoundary
            then []
            else do
              -- List
              (sTop, op) <- maybeToList $ evalUnspreadMiddle eval (sl, tm, sm)
              lTop <- evalUnspreadLeft eval (tl, sl) sTop -- Check boundaries TODO
              rTop <- evalUnspreadRight eval (sm, tr) sTop -- TODO boundaris
              pure $ getState lTop sTop rTop op
         where
          getState lTop sTop rTop op = SSOpen (Path (Trans lTop False tlBoundary) (Slice sTop) (Path (Trans rTop tr2nd trBoundary) sr rst)) (op : ops)

-- Keep in mind we start at the very end of the piece to parse.
-- Everytime we thaw a transition we send it the evaluator. Set t2nd upon an unspread, for use later for splits.

heursiticSearchGoalTest
  :: SearchState es es' ns o
  -> Bool
heursiticSearchGoalTest s = case s of
  SSSemiOpen{} -> False
  SSFrozen{} -> False
  SSOpen p _ -> oneChordPerSegment p
   where
    oneChordPerSegment :: Path (Trans es) (Slice ns) -> Bool
    oneChordPerSegment (PathEnd _) = True
    oneChordPerSegment (Path tl _ rst) = tBoundary tl && oneChordPerSegment rst

printPathFromState
  :: (Show es, Show ns)
  => SearchState es es' ns o
  -> String
printPathFromState s = maybe "" show (getPathFromState s)

getOpsFromState
  :: SearchState es es' ns o
  -> [o]
getOpsFromState s = case s of
  SSOpen p d -> d
  SSSemiOpen p m f d -> d
  SSFrozen p -> []

getPathFromState
  :: SearchState es es' ns o
  -> Maybe (Path es ns)
getPathFromState s = case s of
  SSOpen p d -> Just $ transformPath p
  SSSemiOpen p m f d -> Just $ transformPath f
  SSFrozen p -> Nothing
 where
  transformPath
    :: Path (Trans es) (Slice ns)
    -> Path es ns
  transformPath (PathEnd t) = PathEnd (tContent t)
  transformPath (Path t s rst) = Path (tContent t) (sContent s) $ transformPath rst
