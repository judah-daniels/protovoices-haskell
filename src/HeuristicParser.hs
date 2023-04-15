{-# LANGUAGE ScopedTypeVariables #-}

{- | A parser for the protovoice model, representials partial reductions as a search state that can be traversed
    by applying operations such as 'Unspread', 'Unfreeze' and 'Unsplit'.
-}
module HeuristicParser 
  (
    Slice (..)
  , SliceWrapped (..)
  , SliceWrapper (..)
  , sliceWrapper
  , idWrapper 

  , Trans (..)
  , Boundary 

  , SearchState (..)
  , SearchState'

  , ActionSingle (..)
  , ActionDouble (..)

  , exploreStates

  , goalTest
  , goalTestSBS
  , heursiticSearchGoalTest

  , getPathFromState
  , getPathLengthFromState
  , getPathFromState'
  , getSlicesFromState
  , getOpFromState
  , getOpsFromState

  , chordAccuracy
  , guessChords
  , showOp
  )
    where

import Common
import Control.Monad.Except (ExceptT, lift, throwError)
import Data.Bifunctor qualified as BF (second)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (Maybe, catMaybes, fromJust, mapMaybe, maybeToList)
import Musicology.Pitch
import PVGrammar
import Harmony
import Harmony.Params
import Harmony.ChordLabel

-- | Slice datatype for parsing
newtype Slice ns = Slice
  { sContent :: ns
  -- ^ Content of the Slice
  }
  deriving (Eq)

{- | Data type for a slice "wrapped" with additional information. Contains the content alongside
    the guess of the most likely chord label, and our confidence in that label.
-}
data SliceWrapped ns = SliceWrapped
  { sWContent :: ns
  , sLbl :: ChordLabel
  , sLblProb :: Double
  }
  deriving (Eq)

-- | Type for functions that 'wrap' slices with additional information
newtype SliceWrapper ns = SliceWrapper
  { wrapSlice :: ns -> SliceWrapped ns
  -- ^ Converts a slice into a wrapped slice, giving additional information such as a guess of the chord it represents.
  }

idWrapper :: SliceWrapper ns
idWrapper = SliceWrapper{wrapSlice = \n -> SliceWrapped n undefined 1}

instance Show ns => Show (Slice ns) where
  show (Slice ns) = show ns

instance Show ns => Show (SliceWrapped ns) where
  show (SliceWrapped ns _ _) = show ns

-- | Boolean type synonym representing if a transition is a boundary between two segments
type Boundary = Bool

{- | Transition wrapper datatype. Wraps a transition with a flag indicating whether or not the transition
    is the 2nd right parent of a spread, and another flag marking whether or not this transition is at a boundary
    between segments
-}
data Trans es = Trans
  { tContent :: !es
  -- ^ The value inside the transition, i.e. protovoice edges
  , t2nd :: !Bool
  -- ^ Marks if this transition is the 2nd right parent of a spread
  , tBoundary :: !Boundary
  -- ^ Marks whether or not this transition is at a boundary between two segments
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
data SearchState es es' ns o
  = SSFrozen !(Path (Maybe es', Bool) (SliceWrapped ns)) -- Beginning of search - all frozen edges and slices
  | SSSemiOpen -- Point between beginning and end of the path
      { _ssFrozen :: !(Path (Maybe es', Bool) (SliceWrapped ns))
      -- ^ frozen transitions and slices from current point leftward
      , _ssMidSlice :: !(SliceWrapped ns)
      -- ^ the slice at the current posision between gsFrozen and gsOpen
      , _ssOpen :: !(Path (Trans es) (SliceWrapped ns))
      -- ^ non-frozen transitions and slices from current point rightward
      , _ssDeriv :: ![o]
      -- ^ derivation from current reduction to original surface
      }
  | SSOpen !(Path (Trans es) (SliceWrapped ns)) ![o] -- Single path with unfrozen transition,slices and history

type SearchState' = SearchState (Edges SPitch) [Edge SPitch] (Notes SPitch) (Leftmost (Split SPitch) Freeze (Spread SPitch))

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
showOpen path = go 3 path <> "⋉"
 where
  go _ (PathEnd (Trans _ _ True)) = "⌿"
  go _ (PathEnd (Trans _ _ False)) = "-"
  go 0 (Path (Trans _ _ True) a rst) = "⌿" <> show a <> "..."
  go n (Path (Trans _ _ True) a rst) = "⌿" <> show a <> go (n - 1) rst
  go 0 (Path _ a rst) = "-" <> show a <> "..."
  go n (Path _ a rst) = "-" <> show a <> go (n - 1) rst

{- | Returns a list of possible next states given the current state.
     Works by applying the inverse of the following operations, as returned by the protovoiceEvaluator.
 >
 > freeze left:         split left:          split right:         spread:
 > ...=[]——[]——[]—...   ...=[]——[]——[]—...   ...=[]——[]——[]—...   ...=[]——[]——[]—...
 > ...=[]==[]——[]—...        \  /                     \  /             \  /\  /
 >                            []                       []               []——[]
-}
exploreStates
  :: forall es es' ns ns' s f h
   . (Show s, Show f, Show h, Show es, Show es', Show ns, Show ns')
  => SliceWrapper ns
  -> Eval es es' ns ns' (Leftmost s f h)
  -> SearchState es es' ns (Leftmost s f h)
  -> ExceptT String IO [SearchState es es' ns (Leftmost s f h)]
  -- ^ Either a error message or a list of possible next states
exploreStates wrap eval state = do
  -- lift $ putStrLn "_________________________________________________________"
  -- lift $ putStrLn $ "\n Exploring state: " <> show state <> "\n"
  case state of
    SSFrozen frozen -> case frozen of
      -- Only one trasition: we unfreeze and terminate
      PathEnd t -> do
        -- lift $ putStrLn "1 frozen transition: "
        pure $ genState <$> actions
       where
        actions = collectUnfreezeSingle Start t Stop True
        genState (ActionSingle (sl, top, sr) op) = SSOpen (PathEnd top) [LMSingle op]
      -- Multiple transitions: unfreeze first and continue
      Path t slice rest -> do
        -- lift $ putStrLn "1+ frozen transitions only:"
        pure $ genState <$> actions
       where
        actions = collectUnfreezeSingle Start t (Inner $ sWContent slice) True
        genState (ActionSingle (sl, top, sr) op) = SSSemiOpen rest slice (PathEnd top) [LMSingle op]

    -- Case: Every transition is unfrozen
    SSOpen open ops -> reductions
     where
      reductions = case open of
        -- A single transition - No operations
        PathEnd _ -> do
          -- lift $ putStrLn "0 Frozen, 1 open: No operations\n"
          pure []
        -- Two open transitions: unsplit single
        Path tl slice (PathEnd tr) -> do
          -- lift $ putStrLn "2 open transitions only: \n"
          pure $ genState <$> actions
         where
          actions = collectUnsplitSingle Start tl (sWContent slice) tr Stop
          genState (ActionSingle (sl, top, sr) op) =
            SSOpen (PathEnd top) (LMSingle op : ops)

        -- Three open transitions: unsplitLeft, unsplitRight, or unspread (double ops)
        Path tl sl (Path tm sr rst) -> do
          -- lift $ putStrLn "3+ Open transitions:"
          pure $ genState <$> actions
         where
          actions = collectDoubles Start tl (sWContent sl) tm (sWContent sr) (BF.second sWContent rst)
          genState (ActionDouble (_, topl, tops, topr, _) op) =
            SSOpen (Path topl (wrapSlice wrap tops) (pathSetHead rst topr)) (LMDouble op : ops)

    -- Mid Parse
    --
    SSSemiOpen frozen midSlice open ops ->
      let frozenBoundary = snd $ pathHead frozen
       in case open of
            -- Only one open transition: unfreeze
            PathEnd topen -> case frozen of
              PathEnd tfrozen -> do
                -- lift $ putStrLn "1 Open Transition, 1 frozen transition: \n"
                pure $ genState <$> actions
               where
                actions = collectUnfreezeLeft Start tfrozen (sWContent midSlice) topen Stop True
                genState (ActionDouble (sl, tl, slice, tr, st) op) =
                  SSOpen
                    (Path tl (wrapSlice wrap slice) (PathEnd tr))
                    (LMDouble op : ops)
              Path tfrozen sfrozen rstFrozen -> do
                -- lift $ putStrLn "1 Open Transition, 1+ frozen transition: \n"
                pure $ genState <$> actions
               where
                actions = collectUnfreezeLeft (Inner $ sWContent sfrozen) tfrozen (sWContent midSlice) topen Stop True
                genState (ActionDouble (sl, tl, slice, tr, st) op) =
                  SSSemiOpen rstFrozen sfrozen (Path tl midSlice open) (LMDouble op : ops)

            -- Two Open transitions: unfreeze or unsplit single
            Path topenl sopen (PathEnd topenr@(Trans _ _ topenrBoundary)) -> do
              let unsplitActions = Right <$> collectUnsplitSingle (Inner $ sWContent midSlice) topenl (sWContent sopen) topenr Stop
               in case frozen of
                    PathEnd tfrozen -> do
                      -- lift $ putStrLn "2 Open Transitions, 1 frozen transition: \n"
                      pure $ genState <$> (unfreezeActions <> unsplitActions)
                     where
                      unfreezeActions =
                        Left
                          <$> collectUnfreezeLeft Start tfrozen (sWContent midSlice) topenl Stop topenrBoundary

                      genState action = case action of
                        Left (ActionDouble (_, unfrozen, _, _, _) op) ->
                          SSOpen (Path unfrozen midSlice open) (LMDouble op : ops)
                        Right (ActionSingle (parentL, parent, parentR) op) ->
                          SSSemiOpen frozen midSlice (PathEnd parent) (LMSingle op : ops) -- change midSlice?
                    Path tfrozen sfrozen rstFrozen -> do
                      -- lift $ putStrLn "2 Open Transitions, 1+ frozen transitions: \n"
                      pure $ genState <$> (unfreezeActions <> unsplitActions)
                     where
                      unfreezeActions =
                        Left
                          <$> collectUnfreezeLeft (Inner $ sWContent sfrozen) tfrozen (sWContent midSlice) topenl (Inner (sWContent sopen)) topenrBoundary
                      genState action = case action of
                        Left (ActionDouble (_, unfrozen, _, _, _) op) ->
                          SSSemiOpen rstFrozen sfrozen (Path unfrozen midSlice open) (LMDouble op : ops)
                        Right (ActionSingle (_, parent, _) op) ->
                          SSSemiOpen frozen midSlice (PathEnd parent) (LMSingle op : ops)

            -- More than two open transitions
            Path topenl sopenl (Path topenm@(Trans _ _ topenmBoundary) sopenr rstOpen) -> do
              let doubleActions =
                    Right
                      <$> collectDoubles
                        (Inner $ sWContent midSlice) -- E C C G
                        topenl -- boundary
                        (sWContent sopenl) -- D C Bb F#
                        topenm -- no boundary
                        (sWContent sopenr) -- D C A F#
                        (BF.second sWContent rstOpen)
               in case frozen of
                    PathEnd tfrozen -> do
                      -- lift $ putStrLn "2+ Open Transitions, 1 frozen transition Left: \n"
                      pure $ genState <$> (doubleActions <> unfreezeActions)
                     where
                      unfreezeActions =
                        Left <$> collectUnfreezeLeft Start tfrozen (sWContent midSlice) topenl (Inner $ sWContent sopenl) topenmBoundary

                      genState action = case action of
                        Left (ActionDouble (_, unfrozen, _, _, _) op) ->
                          SSOpen (Path unfrozen midSlice open) (LMDouble op : ops)
                        Right (ActionDouble (_, topl, tops, topr, _) op) ->
                          SSSemiOpen frozen midSlice (Path topl (wrapSlice wrap tops) (pathSetHead rstOpen topr)) (LMDouble op : ops)
                    Path tfrozen sfrozen rstFrozen -> do
                      -- lift $ putStrLn "2+ Open Transitions, 1+ frozen transition left:\n"
                      pure $ genState <$> (doubleActions <> unfreezeActions)
                     where
                      unfreezeActions =
                        Left <$> collectUnfreezeLeft Start tfrozen (sWContent midSlice) topenl (Inner $ sWContent sopenl) topenmBoundary

                      genState action = case action of
                        Left (ActionDouble (_, unfrozen, _, _, _) op) ->
                          SSSemiOpen rstFrozen sfrozen (Path unfrozen midSlice open) (LMDouble op : ops)
                        Right (ActionDouble (_, topl, tops, topr, _) op) ->
                          SSSemiOpen frozen midSlice (Path topl (wrapSlice wrap tops) (pathSetHead rstOpen topr)) (LMDouble op : ops)
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
    -> Boundary
    -> [ActionDouble ns es s f h]
  collectUnfreezeLeft sFrozen (tFrozen, frozenBoundary) sl (Trans tl _ tlBoundary) sr trBoundary
    | allowUnfreeze frozenBoundary tlBoundary trBoundary =
        mapMaybe
          getAction
          (evalUnfreeze eval sFrozen tFrozen (Inner sl) False)
    | otherwise = []
   where
    getAction (thawed, op) = case op of
      LMDouble dop ->
        Just $ ActionDouble (sFrozen, Trans thawed False frozenBoundary, sl, Trans tl False tlBoundary, sr) dop
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

  collectDoubles sstart tl@(Trans _ _ tlBoundary) sl tm@(Trans _ _ tmBoundary) sr rst = leftUnsplits <> rightUnsplits <> unspreads
   where
    (tr@(Trans _ _ trBoundary), send) = case rst of
      PathEnd t -> (t, Stop)
      Path t s _ -> (t, Inner s)
    leftUnsplits = collectUnsplitLeft sstart tl sl tm sr tr send
    rightUnsplits = collectUnsplitRight sstart tl sl tm sr tr send
    unspreads = collectUnspreads sstart tl sl tm sr tr send

  -- \| Defines if an unfreeze is allowed given the boundaries of the frozen transition, and of the nearest two neighboring transistions
  --   > ...=[]—f—[]—l—[]—r-...
  --
  allowUnfreeze
    :: Bool -> Bool -> Bool -> Bool
  allowUnfreeze frozenBoundary lBoundary rBoundary =
    rBoundary || not (lBoundary || frozenBoundary)

printPathFromState
  :: (Show es, Show ns)
  => SearchState es es' ns o
  -> String
printPathFromState s = maybe "" show (getPathFromState s)

-- | Returns a list of all operations that have been taken up to the state given
getOpsFromState
  :: SearchState es es' ns o
  -> [o]
getOpsFromState s = case s of
  SSOpen p d -> d
  SSSemiOpen p m f d -> d
  SSFrozen p -> []

-- | Returns the latest operation that has been applied, or Nothing if no operations have been applied
getOpFromState
  :: SearchState es es' ns o
  -> Maybe o
getOpFromState s = case s of
  SSOpen p (d : _) -> Just d
  SSSemiOpen p m f (d : _) -> Just d
  SSFrozen p -> Nothing

getPathLengthFromState
  :: SearchState es es' ns o
  -> Int
getPathLengthFromState (SSOpen p d) = pathLen p
getPathLengthFromState (SSSemiOpen p m f d) = pathLen f + pathLen p
getPathLengthFromState (SSFrozen p) = pathLen p

getSlicesFromState
  :: SearchState es es' ns o
  -> Maybe [SliceWrapped ns]
getSlicesFromState s = case s of
  SSOpen p d -> Just $ pathBetweens p
  SSSemiOpen p m f d -> Just $ [m] <> pathBetweens f
  SSFrozen p -> Nothing
 where
  transformPath
    :: Path (Trans es) (SliceWrapped ns)
    -> Path es ns
  transformPath (PathEnd t) = PathEnd (tContent t)
  transformPath (Path t s rst) = Path (tContent t) (sWContent s) $ transformPath rst

getPathFromState
  :: SearchState es es' ns o
  -> Maybe (Path es ns)
getPathFromState s = case s of
  SSOpen p d -> Just $ transformPath p
  SSSemiOpen p m f d -> Just $ transformPath f
  SSFrozen p -> Nothing
 where
  transformPath
    :: Path (Trans es) (SliceWrapped ns)
    -> Path es ns
  transformPath (PathEnd t) = PathEnd (tContent t)
  transformPath (Path t s rst) = Path (tContent t) (sWContent s) $ transformPath rst

-- Get path from state, keeping chord label information
getPathFromState'
  :: SearchState es es' ns o
  -> Maybe (Path es (SliceWrapped ns))
getPathFromState' s = case s of
  SSOpen p d -> Just $ transformPath p
  SSSemiOpen p m f d -> Just $ transformPath f
  SSFrozen p -> Nothing
 where
  transformPath
    :: Path (Trans es) (SliceWrapped ns)
    -> Path es (SliceWrapped ns)
  transformPath (PathEnd t) = PathEnd (tContent t)
  transformPath (Path t s rst) = Path (tContent t) s $ transformPath rst

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

-- HELPER FUNCTIONS
showOp [] = ""
showOp (x : _) = case x of
  LMDouble y -> show y
  LMSingle y -> show y

-- | Returns 'True' if the parse is complete, and has been reduced to only one slice
goalTestSBS (SSOpen p _) | pathLen p == 2 = True
goalTestSBS _ = False

-- | Returns 'True' if the parse is complete and has been reduced to only one slice per segment
goalTest :: [ChordLabel] -> SearchState es es' ns o -> Bool
goalTest chordLabels (SSOpen p _) = pathLen p - 1 == length chordLabels
goalTest chordlabels _ = False

-- | Goal test for a search: Only complete once there is a single slice per segment
heursiticSearchGoalTest
  :: SearchState es es' ns o
  -> Bool
heursiticSearchGoalTest s = case s of
  SSSemiOpen{} -> False
  SSFrozen{} -> False
  SSOpen p _ -> oneChordPerSegment p
   where
    oneChordPerSegment :: Path (Trans es) (SliceWrapped ns) -> Bool
    oneChordPerSegment (PathEnd _) = True
    oneChordPerSegment (Path tl _ rst) = tBoundary tl && oneChordPerSegment rst

-- | Returns a 'SliceWrapper' that guesses the most likely chord for a slice
sliceWrapper :: SliceWrapper (Notes SPitch)
sliceWrapper = SliceWrapper $ \ns -> let (l, p) = mostLikelyLabelFromSliceWithProb ns in SliceWrapped ns l p

-- | Returns the most likely chord labels for each input group of notes
guessChords :: [Notes SPitch] -> [ChordLabel]
guessChords slices = sLbl <$> (wrapSlice (SliceWrapper $ \ns -> let (l, p) = mostLikelyLabelFromSliceWithProb ns in SliceWrapped ns l p) <$> slices)

-- | Calculate a naive accuracy metric for two lists using equality
chordAccuracy :: (Eq a, Show a) => [a] -> [a] -> Double
chordAccuracy guesses truth = fromIntegral (numMatches guesses truth) / fromIntegral (length truth)
 where
  numMatches [] [] = 0
  numMatches (x : xs) (y : ys)
    | x == y = 1 + numMatches xs ys
    | otherwise = numMatches xs ys
  numMatches _ _ = error $ show guesses <> show truth
