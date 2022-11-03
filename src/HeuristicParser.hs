{-# LANGUAGE ScopedTypeVariables #-}

module HeuristicParser where

import Common
import Data.Maybe

--
--data Path a e = Path !a !e !(Path a e)
--              | PathEnd !a
-- e is the content of a slice
-- a is the content of a transition

-- |
newtype Slice ns = Slice {sContent :: ns}

data Trans es = Trans
  { tContent :: !es,
    t2nd :: !Bool,
    tBoundary :: !Bool
  }
  deriving (Show)

-- data SearchState ns es o = SearchState {
--     ssSurface :: Path (Trans es) (Slice ns),
--     ssOperations :: [o]
--   }

-- | The state of the search between steps
-- e' : Unparsed/frozen transitions
data SearchState es es' ns o
  = SSFrozen !(Path (Maybe es', Bool) (Slice ns)) -- Beginning of search - all frozen edges and slices
  | SSSemiOpen -- Point between beginning and end of the path
      { -- | frozen transitions and slices from current point leftward
        _ssFrozen :: !(Path (Maybe es', Bool) (Slice ns)),
        -- | the slice at the current posision between gsFrozen and gsOpen
        _ssMidSlice :: !(Slice ns),
        -- | non-frozen transitions and slices from current point rightward
        _ssOpen :: !(Path (Trans es) (Slice ns)),
        -- | derivation from current reduction to original surface
        _ssDeriv :: ![o]
      }
  | SSOpen !(Path (Trans es) (Slice ns)) ![o] -- Single path with unfrozen transition,slices and history
  --

--Mainloop with an agenda
--

-- Main loop
search :: Int
search = undefined

exploreStates :: forall es es' ns o. SearchState es es' ns o -> Eval es es' ns ns o -> [SearchState es es' ns o]
exploreStates state eval = case state of
  SSFrozen path -> case path of
    PathEnd (t, boundary) -> map genState thawed
      where
        thawed = evalThaw eval Start t Stop True
        -- genState :: (es, o) -> SearchState es es' ns o
        genState (t, o) = SSOpen (PathEnd (Trans t False boundary)) [o]
    Path (t, boundary) slice rest -> map genState thawed
      where
        thawed = evalThaw eval Start t (Inner $ sContent slice) False
        genState (t, o) = SSSemiOpen rest slice (PathEnd (Trans t False boundary)) [o]
  SSSemiOpen frozen midSlice open ops -> thawOps <> reductions -- we can either unfreeze, or apply an operation to the open part
    where
      thawOps = case frozen of
        PathEnd (t, boundary) -> map genState thawed
          where
            thawed = evalThaw eval Start t Stop False
            genState (t, op) = SSOpen (Path (Trans t False boundary) midSlice open) (op : ops)
        Path (t, boundary) frozenSlice rest -> map genState thawed
          where
            thawed = evalThaw eval Start t Stop False
            genState (t, op) = SSSemiOpen rest frozenSlice (Path (Trans t False boundary) midSlice open) (op : ops)

      -- data Eval e e' a a' v = Eval
      --   { evalVertMiddle :: !(VertMiddle e a v)
      --   , evalVertLeft   :: !(VertLeft e a)
      --   , evalVertRight  :: !(VertRight e a)
      --   , evalMerge      :: !(Merge e a v)
      --   , evalThaw :: !(StartStop a -> Maybe e' -> StartStop a -> IsLast -> [(e, v)])
      --   , evalSlice      :: !(a' -> a)
      --   }
      -- unspread 3 trans
      -- unsplit 2 trans
      -- unfreeze 1 single frozen trans

      reductions :: [SearchState es es' ns o]
      reductions = case open of
        -- A single transition - No operations
        PathEnd _ -> []
        -- Two open transitions: merge
        Path (Trans tl tl2nd tlBoundary) (Slice slice) (PathEnd (Trans tr _ trBoundary)) -> if tlBoundary then [] else map genState merged
          where
            merged = evalMerge eval (Inner $ sContent midSlice) tl slice tr Stop LeftOnly
            genState (parent, op) = SSSemiOpen frozen midSlice (PathEnd (Trans parent tl2nd trBoundary)) (op : ops)

        -- Three open transitions: mergeleft mergeRight or verticalise
        Path (Trans tl tl2nd tlBoundary) (Slice sl) (Path (Trans tm tm2nd tmBoundary) (Slice sr) (PathEnd (Trans tr tr2nd trBoundary))) -> leftMergeStates <> rightMergeStates -- <> vertStates
          where

            -- TODO consider 2nd and boundary
            leftMergeStates = map genState leftMerges 
              where 
                leftMerges = evalMerge eval (Inner $ sContent midSlice) tl sl tm (Inner sr) LeftOnly
                genState (parent, op) = SSSemiOpen frozen midSlice (Path (Trans parent tl2nd tlBoundary) (Slice sr) (PathEnd (Trans tr tr2nd trBoundary))) (op : ops)

            -- TODO consider 2nd and boundary
            rightMergeStates = if not tm2nd || tmBoundary then [] else map genState rightMerges 
              where 
                rightMerges = evalMerge eval (Inner sl) tl sl tm (Inner sr) LeftOnly
                genState (parent, op) = SSSemiOpen frozen midSlice (Path (Trans tl tl2nd tlBoundary) (Slice sl) (PathEnd (Trans parent True trBoundary))) (op : ops)


  -- { evalVertMiddle :: !(VertMiddle e a v)
  -- , evalVertLeft   :: !(VertLeft e a)
  -- , evalVertRight  :: !(VertRight e a)
            vertStates = if tl2nd then [] else
              do -- List
                (sTop, op) <- maybeToList $ evalVertMiddle eval (sl, tm, sr)
                lTop       <- evalVertLeft eval (tl, sl) sTop -- Check boundaries TODO
                rTop       <- evalVertRight eval (sr, tr) sTop -- TODO boundaris
                pure $ getState lTop sTop rTop op

                where
                      getState lTop sTop rTop op = Just $ SSSemiOpen frozen midSlice (Path (Trans lTop False False) (Slice sTop) (PathEnd (Trans rTop False False))) (op : ops)
                          -- (sstart, Trans lTop False, sTop, Trans rTop True, send)
                          -- dop
-- data ActionDouble a e s f h = ActionDouble ( StartStop a
--                                            , Trans e
--                                            , a
--                                            , Trans e
--                                            , StartStop a
--                                            )
--                                            (LeftmostDouble s f h)
--   deriving Show
--
                -- leftVerts = evalVertLeft (tl, (Inner $ sContent midSlice) )
                -- verts = evalMerge eval (Inner $ sContent midSlice) tl sl tm (Inner sr) LeftOnly
                --
            genState (parent, op) = SSSemiOpen frozen midSlice (PathEnd (Trans parent tl2nd trBoundary)) (op : ops)


        Path (Trans tl tl2nd tlBoundary) (Slice sl) (Path (Trans tm tm2nd tmBoundary) (Slice sm) (Path (Trans tr tr2nd trBoundary) sr rst)) -> []
  -- type Merge e a v
  --   = StartStop a -> e -> a -> e -> StartStop a -> SplitType -> [(e, v)]
  --   split type determines if its a left right or single split.
  -- --
  --         Path (Trans t1 tl2nd tlBoundary ) (Slice slice) (PathEnd (Trans t2 _ trBoundary)) -> if tlBoundary then [] else map genState merged -- unsplit here
  --           where
  --             merged = evalMerge eval (Inner $ sContent midSlice) t1 slice t2 Stop LeftOnly
  --             -- genState (es, o) = SSSemiOpen rest frozenSlice (Path (Trans es False boundary) midSlice open) (o:ops)
  --             genState (parent, o) = SSSemiOpen frozen midSlice (PathEnd (Trans parent tl2nd trBoundary)) (o:ops)
  --
  --         Path t1 s1 (Path t2 s2 p) -> undefined -- unsplit or unspread here

  SSOpen path ops -> undefined

-- where
--   collectMergeSingle
--     :: (  StartStop ns
--        -> Trans es
--        -> ns
--        -> Trans es
--        -> StartStop ns
--        -> [SearchState es es' ns o]
--        )
--   collectMergeSingle sl (Trans tl tl2nd tlBoundary) sm (Trans tr tr2nd trBoundary) sr =
--     map genState merged
--    where
--      merged = evalMerge eval sl tl sm tr sr LeftOnly
--      genState (parent, o) = SSSemiOpen frozen

-- Keep in mind we start at the very end of the piece to parse.
-- Everytime we thaw a transition we send it the evaluator. Set t2nd upon an unspread, for use later for splits.
