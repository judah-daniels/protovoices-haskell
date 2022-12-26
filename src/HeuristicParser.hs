{-# LANGUAGE ScopedTypeVariables #-}

module HeuristicParser where

import           Common
import           Data.Maybe

newtype Slice ns = Slice
  { sContent :: ns
  }
  deriving (Eq)

instance Show ns => Show (Slice ns) where
  show (Slice ns) = show ns

data Trans es = Trans
  { tContent  :: !es,
    t2nd      :: !Bool,
    tBoundary :: !Bool
  }
  deriving (Eq, Show)


-- | The state of the search between steps
-- es' : Unparsed/frozen transitions
-- es  : Unfrozen transitions
-- ns  : Evaluated slice content
-- o   : Operation type
data SearchState es es' ns o
  = SSFrozen !(Path (Maybe es', Bool) (Slice ns)) -- Beginning of search - all frozen edges and slices
  | SSSemiOpen -- Point between beginning and end of the path
      { -- | frozen transitions and slices from current point leftward
        _ssFrozen   :: !(Path (Maybe es', Bool) (Slice ns)),
        -- | the slice at the current posision between gsFrozen and gsOpen
        _ssMidSlice :: !(Slice ns),
        -- | non-frozen transitions and slices from current point rightward
        _ssOpen     :: !(Path (Trans es) (Slice ns)),
        -- | derivation from current reduction to original surface
        _ssDeriv    :: ![o]
      }
  | SSOpen !(Path (Trans es) (Slice ns)) ![o] -- Single path with unfrozen transition,slices and history

instance (Show es, Show es', Show ns, Show o) => Show (SearchState es es' ns o) where
  show state = case state of
    SSFrozen path -> "\n" ++ "Frozen: " ++ show path
    SSSemiOpen frozen midSlice open deriv 
      -> "\n" 
      ++ "SemiOpen: \n" 
      ++ "Frozen: " ++ show frozen 
      ++ "\nMid: " ++ show midSlice 
      ++ "\nOpen : " ++ show open
    SSOpen path deriv-> "\n" ++ "Open: " ++ "\nPath: " ++ show path ++ "\nSteps: " ++ show deriv


-- | Returns a list of possible next states given the current state.
exploreStates :: forall es es' ns ns' o. Eval es es' ns ns' o -> SearchState es es' ns o -> [SearchState es es' ns o]
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
        Path (Trans tl tl2nd tlBoundary) (Slice slice) (PathEnd (Trans tr _ trBoundary)) -> if tlBoundary then [] else map genState merged
          where
            merged = evalUnsplit eval (Inner $ sContent midSlice) tl slice tr Stop SingleOfOne
            genState (parent, op) = SSSemiOpen frozen midSlice (PathEnd (Trans parent tl2nd trBoundary)) (op : ops)

        -- Three open transitions: mergeleft mergeRight or verticalise
        Path (Trans tl tl2nd tlBoundary) (Slice sl) (Path (Trans tm tm2nd tmBoundary) (Slice sm) (PathEnd (Trans tr tr2nd trBoundary))) -> leftMergeStates <> rightMergeStates <> vertStates
          where
            -- TODO consider 2nd and boundary
            leftMergeStates = map genState leftMerges
              where
                leftMerges = evalUnsplit eval (Inner $ sContent midSlice) tl sl tm (Inner sm) LeftOfTwo
                genState (parent, op) = SSSemiOpen frozen midSlice (Path (Trans parent tl2nd tlBoundary) (Slice sm) (PathEnd (Trans tr tr2nd trBoundary))) (op : ops)

            -- TODO consider 2nd and boundary
            rightMergeStates = if not tm2nd || tmBoundary then [] else map genState rightMerges
              where
                rightMerges = evalUnsplit eval (Inner sl) tm sm tr Stop RightOfTwo
                genState (parent, op) = SSSemiOpen frozen midSlice (Path (Trans tl tl2nd tlBoundary) (Slice sl) (PathEnd (Trans parent True trBoundary))) (op : ops)

            vertStates =
              if tl2nd || tmBoundary
                then []
                else do
                  -- List
                  (sTop, op) <- maybeToList $ evalUnspreadMiddle eval (sl, tm ,sm)
                  lTop <- evalUnspreadLeft eval (tl, sl) sTop -- Check boundaries TODO
                  rTop <- evalUnspreadRight eval (sm, tr) sTop -- TODO boundaris
                  pure $ getState lTop sTop rTop op
              where
                getState lTop sTop rTop op = SSSemiOpen frozen midSlice (Path (Trans lTop False False) (Slice sTop) (PathEnd (Trans rTop False False))) (op : ops)

        Path (Trans tl tl2nd tlBoundary) (Slice sl) (Path (Trans tm tm2nd tmBoundary) (Slice sm) (Path (Trans tr tr2nd trBoundary) sr rst)) -> leftMergeStates <> rightMergeStates <> vertStates
          where
            -- TODO consider 2nd and boundary
            leftMergeStates = map genState leftMerges
              where
                leftMerges = evalUnsplit eval (Inner $ sContent midSlice) tl sl tm (Inner sm) LeftOfTwo
                genState (parent, op) = SSSemiOpen frozen midSlice (Path (Trans parent tl2nd tlBoundary) (Slice sm) (PathEnd (Trans tr tr2nd trBoundary))) (op : ops)

            -- TODO consider 2nd and boundary
            rightMergeStates = if not tm2nd || tmBoundary then [] else map genState rightMerges
              where
                rightMerges = evalUnsplit eval (Inner sl) tl sl tm (Inner sm) RightOfTwo
                genState (parent, op) = SSSemiOpen frozen midSlice (Path (Trans tl tl2nd tlBoundary) (Slice sl) (PathEnd (Trans parent True trBoundary))) (op : ops)

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
                getState lTop sTop rTop op = SSSemiOpen frozen midSlice (Path (Trans lTop False False) (Slice sTop) (Path (Trans rTop tr2nd trBoundary) sr rst)) (op : ops)



  SSOpen open ops -> reductions -- we can either unfreeze, or apply an operation to the open part
    where
      reductions :: [SearchState es es' ns o]
      reductions = case open of
        -- A single transition - No operations
        PathEnd _ -> []
        -- Two open transitions: merge
        Path (Trans tl tl2nd tlBoundary) (Slice slice) (PathEnd (Trans tr _ trBoundary)) -> if tlBoundary then [] else map genState merged
          where
            merged = evalUnsplit eval Start tl slice tr Stop SingleOfOne -- Start as left slice correct?
            genState (parent, op) = SSOpen (PathEnd (Trans parent tl2nd trBoundary)) (op : ops)

        -- Three open transitions: mergeleft mergeRight or verticalise
        Path (Trans tl tl2nd tlBoundary) (Slice sl) (Path (Trans tm tm2nd tmBoundary) (Slice sm) (PathEnd (Trans tr tr2nd trBoundary))) -> leftMergeStates <> rightMergeStates <> vertStates
          where
            -- TODO consider 2nd and boundary
            leftMergeStates = map genState leftMerges
              where
                leftMerges = evalUnsplit eval Start tl sl tm (Inner sm) LeftOfTwo
                genState (parent, op) = SSOpen (Path (Trans parent tl2nd tlBoundary) (Slice sm) (PathEnd (Trans tr tr2nd trBoundary))) (op : ops)

            -- TODO consider 2nd and boundary
            rightMergeStates = if not tm2nd || tmBoundary then [] else map genState rightMerges
              where
                rightMerges = evalUnsplit eval (Inner sl) tl sl tm (Inner sm) RightOfTwo
                genState (parent, op) = SSOpen (Path (Trans tl tl2nd tlBoundary) (Slice sl) (PathEnd (Trans parent True trBoundary))) (op : ops)

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
                getState lTop sTop rTop op = SSOpen (Path (Trans lTop False False) (Slice sTop) (PathEnd (Trans rTop False False))) (op : ops)

        Path (Trans tl tl2nd tlBoundary) (Slice sl) (Path (Trans tm tm2nd tmBoundary) (Slice sm) (Path (Trans tr tr2nd trBoundary) sr rst)) -> leftMergeStates <> rightMergeStates <> vertStates
          where
            -- TODO consider 2nd and boundary
            leftMergeStates = map genState leftMerges
              where
                leftMerges = evalUnsplit eval Start tl sl tm (Inner sm) LeftOfTwo
                genState (parent, op) = SSOpen (Path (Trans parent tl2nd tlBoundary) (Slice sm) (PathEnd (Trans tr tr2nd trBoundary))) (op : ops)

            -- TODO consider 2nd and boundary
            rightMergeStates = if not tm2nd || tmBoundary then [] else map genState rightMerges
              where
                rightMerges = evalUnsplit eval (Inner sl) tl sl tm (Inner sm) RightOfTwo
                genState (parent, op) = SSOpen (Path (Trans tl tl2nd tlBoundary) (Slice sl) (PathEnd (Trans parent True trBoundary))) (op : ops)

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

getOpsFromState 
  :: SearchState es es' ns o
  -> [o]
getOpsFromState s = case s of 
  SSOpen p d -> d
  SSSemiOpen p m f d -> d 
  SSFrozen p -> []

getPathFromState
  :: SearchState es es' ns o
  -> Path es ns
getPathFromState s = case s of
  SSOpen p d -> transformPath p
  SSSemiOpen p m f d -> undefined 
  SSFrozen p -> undefined
  where
    transformPath
      :: Path (Trans es) (Slice ns)
      -> Path es ns
    transformPath (PathEnd t) = PathEnd (tContent t)
    transformPath (Path t s rst) = Path (tContent t) (sContent s) $ transformPath rst
