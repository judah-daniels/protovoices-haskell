{-# LANGUAGE ScopedTypeVariables #-}

module HeuristicParser where

import Common
import Data.Maybe
import Debug.Trace

newtype Slice ns = Slice
  { sContent :: ns
  }
  deriving (Eq)

instance Show ns => Show (Slice ns) where
  show (Slice ns) = show ns

data Trans es = Trans
  { tContent :: !es
  , t2nd :: !Bool
  , tBoundary :: !Boundary
  }
  deriving (Eq)

instance Show es => Show (Trans es) where
  show (Trans t t2 True) = "|" <> show t
  show (Trans t t2 False) = show t

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
showFrozen path = "⋊" <> go 1 path
 where
  go _ (PathEnd (_, True)) = "≠"
  go _ (PathEnd (_, False)) = "="
  go 0 (Path (_, True) a rst) = "..." <> show a <> "≠"
  go n (Path (_, True) a rst) = go (n - 1) rst <> show a <> "≠"
  go 0 (Path (_, False) a rst) = "..." <> show a <> "="
  go n (Path (_, False) a rst) = go (n - 1) rst <> show a <> "="

-- | Helper function for showing the open part of a piece.
showOpen :: Show slc => Path (Trans es') slc -> String
showOpen path = go 2 path <> "⋉"
 where
  go _ (PathEnd (Trans _ _ True)) = "⌿"
  go _ (PathEnd (Trans _ _ False)) = "-"
  go 0 (Path (Trans _ _ True) a rst) = "⌿" <> show a <> "..."
  go n (Path (Trans _ _ True) a rst) = "⌿" <> show a <> go (n - 1) rst
  go 0 (Path _ a rst) = "-" <> show a <> "..."
  go n (Path _ a rst) = "-" <> show a <> go (n - 1) rst

{-
 > freeze left:         split left:          split right:         spread:
 > ...=[]——[]——[]—...   ...=[]——[]——[]—...   ...=[]——[]——[]—...   ...=[]——[]——[]—...
 > ...=[]==[]——[]—...        \  /                     \  /             \  /\  /
 >                            []                       []               []——[]
-}

-- | Returns a list of possible next states given the current state.
exploreStates
  :: forall es es' ns ns' s f h
   . (Show s, Show f, Show h, Show es, Show es', Show ns, Show ns')
  => Eval es es' ns ns' (Leftmost s f h)
  -> SearchState es es' ns (Leftmost s f h)
  -> [SearchState es es' ns (Leftmost s f h)]
exploreStates eval state = case state of
  SSFrozen frozen -> case frozen of
    -- Only one trasition: we unfreeze and terminate
    PathEnd t ->
      trace ("Evaluating :" <> show state <> "\n  1 frozen transition: ") genState <$> actions
     where
      actions = collectUnfreezeSingle Start t Stop True
      genState (ActionSingle (sl, top, sr) op) = SSOpen (PathEnd top) [LMSingle op]
    -- Multiple transitions: unfreeze first and continue
    Path t slice rest ->
      trace
        ("Evaluating:" <> show state <> "\n  1+ frozen transitions only: \n ")
        genState
        <$> actions
     where
      actions = collectUnfreezeSingle Start t (Inner $ sContent slice) True
      genState (ActionSingle (sl, top, sr) op) = SSSemiOpen rest slice (PathEnd top) [LMSingle op]

  -- Case: Every transition is unfrozen
  SSOpen open ops -> trace "ssopen" reductions -- we can either unfreeze, or apply an operation to the open part
   where
    reductions :: [SearchState es es' ns (Leftmost s f h)]
    reductions = case open of
      -- A single transition - No operations
      PathEnd _ -> trace "  0 Frozen, 1 open: No operations\n" []
      -- Two open transitions: unsplit single
      Path tl (Slice slice) (PathEnd tr) ->
        trace
          ("Evaluating:" <> show state <> "\n  2 open transitions only: \n  Unsplit operations: " <> (show (actions)))
          genState
          <$> actions
       where
        actions = collectUnsplitSingle Start tl slice tr Stop
        genState (ActionSingle (sl, top, sr) op) = SSOpen (PathEnd top) (LMSingle op : ops)

      -- Three open transitions: mergeleft mergeRight or verticalise
      Path tl (Slice sl) (Path tm (Slice sm) rst) ->
        trace
          ("Evaluating:" <> show state <> "\n  3+ Open transitions: \n  Actions: " <> show actions)
          genState
          <$> actions
       where
        actions = collectDoubles Start tl sl tm sm rst
        genState (ActionDouble (sl, tl, slice, tr, st) op) =
          SSOpen
            ( Path
                tl
                (Slice slice)
                ( Path
                    tr
                    (Slice sm)
                    (pathSetHead rst tr)
                )
            )
            (LMDouble op : ops)

  -- Mid Parse
  --
  SSSemiOpen frozen (Slice midSlice) open ops -> case open of
    -- Only one open transition: unfreeze
    PathEnd topen -> case frozen of
      PathEnd tfrozen ->
        trace
          ( "Evaluating:"
              <> show state
              <> "\n  1 Open Transition, 1 frozen transition: \n  Following States: "
              -- <> show
              -- (genState <$> actions)
          )
          genState
          <$> actions
       where
        actions = collectUnfreezeLeft Start tfrozen midSlice topen Stop
        genState (ActionDouble (sl, tl, slice, tr, st) op) =
          SSOpen
            (Path tl (Slice slice) (PathEnd tr))
            (LMDouble op : ops)
      Path tfrozen (Slice sfrozen) rstFrozen ->
        trace
          ( "Evaluating:"
              <> show state
              <> "\n  1 Open Transition, 1+ frozen transition: \n  Following States: "
              -- <> show
              -- (genState <$> actions)
          )
          genState
          <$> actions
       where
        actions = collectUnfreezeLeft (Inner sfrozen) tfrozen midSlice topen Stop
        genState (ActionDouble (sl, tl, slice, tr, st) op) =
          SSSemiOpen rstFrozen (Slice sfrozen) (Path tl (Slice midSlice) open) (LMDouble op : ops)

    -- Two Open transitions: unfreeze or unsplit single
    Path topenl (Slice sopen) (PathEnd topenr) ->
      let unsplitActions = Right <$> collectUnsplitSingle (Inner midSlice) topenl sopen topenr Stop
       in case frozen of
            PathEnd tfrozen ->
              trace
                ( "Evaluating:"
                    <> show state
                    <> "\n  2 Open Transitions, 1 frozen transition: \n  Following states: "
                    -- <> show (genState <$> (unfreezeActions <> unsplitActions))
                    -- <> "\n  Following Actions: "
                    -- <> show (unfreezeActions <> unsplitActions)
                    <> "\n"
                )
                genState
                <$> (unfreezeActions <> unsplitActions)
             where
              unfreezeActions = Left <$> collectUnfreezeLeft Start tfrozen midSlice topenl Stop
              genState action = case action of
                Left (ActionDouble (_, unfrozen, _, _, _) op) ->
                  SSOpen (Path unfrozen (Slice midSlice) open) (LMDouble op : ops)
                Right (ActionSingle (_, parent, _) op) ->
                  SSSemiOpen frozen (Slice midSlice) (PathEnd parent) (LMSingle op : ops)
            Path tfrozen (Slice sfrozen) rstFrozen ->
              trace
                ( "Evaluating:"
                    <> show state
                    <> "\n  2 Open Transitions, 1+ frozen transitions: \n Following states: "
                    -- <> (show (genState <$> (unfreezeActions <> unsplitActions)))
                    -- <> "\n  Following Actions: "
                    -- <> show (unfreezeActions <> unsplitActions)
                    <> "\n"
                )
                genState
                <$> (unfreezeActions <> unsplitActions)
             where
              unfreezeActions = Left <$> collectUnfreezeLeft (Inner sfrozen) tfrozen midSlice topenl (Inner sopen)
              genState action = case action of
                Left (ActionDouble (_, unfrozen, _, _, _) op) ->
                  SSSemiOpen rstFrozen (Slice sfrozen) (Path unfrozen (Slice midSlice) open) (LMDouble op : ops)
                Right (ActionSingle (_, parent, _) op) ->
                  SSSemiOpen frozen (Slice midSlice) (PathEnd parent) (LMSingle op : ops)
    -- More than two open transitions
    Path topenl (Slice sopenl) (Path topenm (Slice sopenr) rstOpen) ->
      let doubleActions = (Right <$> collectDoubles (Inner midSlice) topenl sopenl topenm sopenr rstOpen) :: [Either (ActionDouble ns es s f h) (ActionDouble ns es s f h)]
       in case frozen of
            PathEnd tfrozen ->
              trace
                ( "  2+ Open Transitions, 1 frozen transition Left: \n  Following states: "
                    -- <> (show (genState <$> doubleActions))
                    -- <> "\n  Following Actions: "
                    -- <> show (unfreezeActions <> doubleActions)
                    <> "\n"
                )
                genState
                <$> (doubleActions <> unfreezeActions)
             where
              unfreezeActions = Left <$> collectUnfreezeLeft Start tfrozen midSlice topenl (Inner sopenl)
              genState action = case action of
                Left (ActionDouble (_, unfrozen, _, _, _) op) ->
                  SSOpen (Path unfrozen (Slice midSlice) open) (LMDouble op : ops)
                Right (ActionDouble (_, topl, tops, topr, _) op) ->
                  SSSemiOpen frozen (Slice midSlice) (Path topl (Slice tops) (pathSetHead rstOpen topr)) (LMDouble op : ops)
            Path tfrozen (Slice sfrozen) rstFrozen ->
              trace
                ( "  2+ Open Transitions, 1+ frozen transition left:\n  Following states: "
                    <> (show (genState <$> doubleActions))
                    -- <> "\n  Following Actions: "
                    -- <> show (unfreezeActions <> doubleActions)
                    <> "\n"
                )
                genState
                <$> (doubleActions <> unfreezeActions)
             where
              unfreezeActions = Left <$> collectUnfreezeLeft (Inner sfrozen) tfrozen midSlice topenl (Inner sopenl)
              genState action = case action of
                Left (ActionDouble (_, unfrozen, _, _, _) op) ->
                  SSSemiOpen rstFrozen (Slice sfrozen) (Path unfrozen (Slice midSlice) open) (LMDouble op : ops)
                Right (ActionDouble (_, topl, tops, topr, _) op) ->
                  SSSemiOpen frozen (Slice midSlice) (Path topl (Slice tops) (pathSetHead rstOpen topr)) (LMDouble op : ops)
 where
  collectUnfreezeSingle
    :: StartStop ns
    -> (Maybe es', Boundary)
    -> StartStop ns
    -> IsLast
    -> [ActionSingle ns es s f]
  collectUnfreezeSingle sl (t, boundary) sr isLast =
    mapMaybe
      getAction
      (evalUnfreeze eval sl t sr isLast)
   where
    getAction (t', op) = case op of
      LMSingle sop -> Just $ ActionSingle (sl, Trans t' False boundary, sr) sop
      LMDouble _ -> Nothing

  collectUnfreezeLeft
    :: StartStop ns
    -> (Maybe es', Boundary)
    -> ns
    -> Trans es
    -> StartStop ns
    -> [ActionDouble ns es s f h]
  collectUnfreezeLeft sl (tl, tlBoundary) sm (Trans tr _ trBoundary) sr =
    mapMaybe
      getAction
      (evalUnfreeze eval sl tl (Inner sm) False)
   where
    getAction (thawed, op) = case op of
      LMDouble dop ->
        Just $ ActionDouble (sl, Trans thawed False tlBoundary, sm, Trans tr False trBoundary, sr) dop
      LMSingle _ -> Nothing

  collectUnsplitSingle
    :: ( StartStop ns
         -> Trans es
         -> ns
         -> Trans es
         -> StartStop ns
         -> [ActionSingle ns es s f]
       )
  collectUnsplitSingle sl (Trans tl _ tlBoundary) sm (Trans tr _ trBoundary) sr
    | tlBoundary && trBoundary = []
    | otherwise =
        mapMaybe getAction $ evalUnsplit eval sl tl sm tr sr SingleOfOne
   where
    getAction (ttop, op) =
      case op of
        LMSingle sop -> Just $ ActionSingle (sl, Trans ttop False (tlBoundary || trBoundary), sr) sop
        LMDouble _ -> Nothing

  collectUnsplitLeft
    :: ( StartStop ns
         -> Trans es
         -> ns
         -> Trans es
         -> ns
         -> Trans es
         -> StartStop ns
         -> [ActionDouble ns es s f h]
       )
  collectUnsplitLeft sstart (Trans tl _ tlBoundary) sl (Trans tm _ tmBoundary) sr tr send
    | tlBoundary && tmBoundary = []
    | otherwise =
        mapMaybe getAction $
          evalUnsplit eval sstart tl sl tm (Inner sr) LeftOfTwo
   where
    getAction (ttop, op) =
      case op of
        LMSingle _ -> Nothing
        LMDouble dop ->
          Just $
            ActionDouble
              (sstart, Trans ttop False (tlBoundary || tmBoundary), sr, tr, send)
              dop

  collectUnsplitRight
    :: ( StartStop ns
         -> Trans es
         -> ns
         -> Trans es
         -> ns
         -> Trans es
         -> StartStop ns
         -> [ActionDouble ns es s f h]
       )
  collectUnsplitRight sstart tl sl (Trans tm m2nd tmBoundary) sr (Trans tr _ trBoundary) send
    | not m2nd = []
    | tmBoundary && trBoundary = []
    | otherwise =
        mapMaybe getAction $
          evalUnsplit eval (Inner sl) tm sr tr send RightOfTwo
   where
    getAction (ttop, op) = case op of
      LMSingle _ -> Nothing
      LMDouble dop ->
        Just $ ActionDouble (sstart, tl, sl, Trans ttop True (tmBoundary || trBoundary), send) dop

  collectUnspreads
    :: ( StartStop ns
         -> Trans es
         -> ns
         -> Trans es
         -> ns
         -> Trans es
         -> StartStop ns
         -> [ActionDouble ns es s f h]
       )
  collectUnspreads sstart (Trans tl _ tlBoundary) sl (Trans tm _ tmBoundary) sr (Trans tr _ trBoundary) send
    | tmBoundary = []
    | otherwise = catMaybes $ do
        -- List
        (sTop, op) <- maybeToList $ evalUnspreadMiddle eval (sl, tm, sr)
        lTop <- evalUnspreadLeft eval (tl, sl) sTop
        rTop <- evalUnspreadRight eval (sr, tr) sTop
        pure $ getAction lTop sTop rTop op
   where
    -- pure $ getAction $ evalUnsplit eval (Inner sl) tm sr tr send RightOfTwo
    getAction lTop sTop rTop op = case op of
      LMSingle _ -> Nothing
      LMDouble dop ->
        Just $
          ActionDouble
            (sstart, Trans lTop False tlBoundary, sTop, Trans rTop True trBoundary, send)
            dop

  collectDoubles sstart tl sl tm sr rst = leftUnsplits <> rightUnsplits <> unspreads
   where
    (tr, send) = case rst of
      PathEnd t -> (t, Stop)
      Path t (Slice s) _ -> (t, Inner s)
    leftUnsplits = collectUnsplitLeft sstart tl sl tm sr tr send
    rightUnsplits = collectUnsplitRight sstart tl sl tm sr tr send
    unspreads = collectUnspreads sstart tl sl tm sr tr send

type Boundary = Bool

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

getPathLengthFromState
  :: SearchState es es' ns o
  -> Int
getPathLengthFromState (SSOpen p d) = pathLen p
getPathLengthFromState (SSSemiOpen p m f d) = pathLen f + pathLen p
getPathLengthFromState (SSFrozen p) = pathLen p

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

-- * Parsing Actions

{- | A parsing action (reduction step) with a single parent transition.
 Combines the parent elements with a single-transition derivation operation.
-}
data ActionSingle ns tr s f
  = ActionSingle
      (StartStop ns, Trans tr, StartStop ns)
      -- ^ parent transition (and adjacent slices)
      (LeftmostSingle s f)
      -- ^ single-transition operation
  deriving (Show)

{- | A parsing action (reduction step) with two parent transitions.
 Combines the parent elements with a double-transition derivation operation.
-}
data ActionDouble ns tr s f h
  = ActionDouble
      ( StartStop ns
      , Trans tr
      , ns
      , Trans tr
      , StartStop ns
      )
      -- ^ parent transitions and slice
      (LeftmostDouble s f h)
      -- ^ double-transition operation
  deriving (Show)

-- | An alias that combines 'ActionSingle' and 'ActionDouble', representing all possible reduction steps.
type Action slc tr s f h = Either (ActionSingle slc tr s f) (ActionDouble slc tr s f h)
