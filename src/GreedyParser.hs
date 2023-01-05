{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{- | This module contains a simple greedy parser for path grammars.
 The grammar is provided by an evaluator ('Eval').
 In addition, the parser takes a policy function
 that picks a reduction option in each step.
-}
module GreedyParser where

-- TODO: add back export list once haddock's ignore-exports works again.
-- ( parseGreedy
-- , pickRandom
-- , parseRandom
-- , parseRandom'
-- ) where

import Common

import Control.Monad.Except
  ( ExceptT
  , MonadError (throwError)
  )
import Control.Monad.IO.Class
  ( MonadIO
  )
import Control.Monad.Trans.Class (lift)
import Data.Maybe
  ( catMaybes
  , mapMaybe
  , maybeToList
  )
import System.Random (initStdGen)
import System.Random.Stateful
  ( StatefulGen
  , newIOGenM
  , uniformRM
  )

-- * Parsing State

{- | A transition during greedy parsing.
 Augments transition data with a flag
 that indicates whether the transition is a transitive right (2nd) parent of a spread.
-}
data Trans tr = Trans
  { _tContent :: !tr
  -- ^ content of the transition
  , _t2nd :: !Bool
  -- ^ flag that indicates (transitive) right parents of spreads
  }
  deriving (Show)

{- | The state of the greedy parse between steps.
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

 This is the 'GSSemiOpen' case:
 The slice at the current pointer (@[3]@)
 is represented as an individual slice (@midSlice@).
 The frozen part is represented by a 'Path' of frozen transitions (@tr'@) and slices (@slc@).
 __in reverse direction__, i.e. from @midslice@ back to ⋊ (excluding ⋊).
 The open part is a 'Path' of open transitions (@tr@) and slices (@slc@)
 in forward direction from @midSlice@ up to ⋉.

 There are two special cases.
 All transitions can be frozen ('GSFrozen'),
 in which case state only contains the backward 'Path' of frozen transitions
 (excluding ⋊ and ⋉):

 > ⋊==[1]==[2]==[3]==⋉
 >                    └ current position
 > represented as: ==[3]==[2]==[1]==

 Or all transitions can be open ('GSOpen'),
 in which case the state is just the forward path of open transitions:

 > ⋊——[1]——[2]——[3]——⋉
 > └ current position
 > represented as: ——[1]——[2]——[3]——

 The open and semiopen case additionally have a list of operations in generative order.
-}
data GreedyState tr tr' slc op
  = GSFrozen !(Path (Maybe tr') slc)
  | GSSemiOpen
      { _gsFrozen :: !(Path (Maybe tr') slc)
      -- ^ frozen transitions and slices from current point leftward
      , _gsMidSlice :: !slc
      -- ^ the slice at the current posision between gsFrozen and gsOpen
      , _gsOpen :: !(Path (Trans tr) slc)
      -- ^ non-frozen transitions and slices from current point rightward
      , _gsDeriv :: ![op]
      -- ^ derivation from current reduction to original surface
      }
  | GSOpen !(Path (Trans tr) slc) ![op]

instance (Show slc, Show o) => Show (GreedyState tr tr' slc o) where
  show (GSFrozen frozen) = showFrozen frozen <> "⋉"
  show (GSOpen open _ops) = "⋊" <> showOpen open -- <> " " <> show ops
  show (GSSemiOpen frozen mid open _ops) =
    showFrozen frozen <> show mid <> showOpen open -- <> " " <> show ops

-- | Helper function for showing the frozen part of a piece.
showFrozen :: Show slc => Path tr' slc -> String
showFrozen path = "⋊" <> go 5 path
 where
  go _ (PathEnd _) = "="
  go 0 (Path _ a rst) = "..." <> show a <> "="
  go n (Path _ a rst) = go (n - 1) rst <> show a <> "="

-- | Helper function for showing the open part of a piece.
showOpen :: Show slc => Path tr slc -> String
showOpen path = go 5 path <> "⋉"
 where
  go _ (PathEnd _) = "-"
  go 0 (Path _ a rst) = "-" <> show a <> "..."
  go n (Path _ a rst) = "-" <> show a <> go (n - 1) rst

-- * Parsing Actions

{- | A parsing action (reduction step) with a single parent transition.
 Combines the parent elements with a single-transition derivation operation.
-}
data ActionSingle slc tr s f
  = ActionSingle
      (StartStop slc, Trans tr, StartStop slc)
      -- ^ parent transition (and adjacent slices)
      (LeftmostSingle s f)
      -- ^ single-transition operation
  deriving (Show)

{- | A parsing action (reduction step) with two parent transitions.
 Combines the parent elements with a double-transition derivation operation.
-}
data ActionDouble slc tr s f h
  = ActionDouble
      ( StartStop slc
      , Trans tr
      , slc
      , Trans tr
      , StartStop slc
      )
      -- ^ parent transitions and slice
      (LeftmostDouble s f h)
      -- ^ double-transition operation
  deriving (Show)

-- | An alias that combines 'ActionSingle' and 'ActionDouble', representing all possible reduction steps.
type Action slc tr s f h = Either (ActionSingle slc tr s f) (ActionDouble slc tr s f h)

-- * Parsing Algorithm

{- | Parse a piece in a greedy fashion.
 At each step, a policy chooses from the possible reduction actions,
 the reduction is applied, and parsing continues
 until the piece is fully reduced or no more reduction operations are available.
 Returns the full derivation from the top (@⋊——⋉@) or an error message.
-}
parseGreedy
  :: forall m tr tr' slc slc' s f h
   . (Monad m, MonadIO m, Show tr', Show slc, Show tr, Show s, Show f, Show h)
  => Eval tr tr' slc slc' (Leftmost s f h)
  -- ^ the evaluator of the grammar to be used
  -> ([Action slc tr s f h] -> ExceptT String m (Action slc tr s f h))
  -- ^ the policy: picks a parsing action from a list of options
  -- (determines the 'Monad' @m@, e.g., for randomness).
  -> Path slc' tr'
  -- ^ the input piece
  -> ExceptT String m (Analysis s f h tr slc)
  -- ^ the full parse or an error message
parseGreedy eval pick input = do
  (top, deriv) <- parse initState
  pure $ Analysis deriv $ PathEnd top
 where
  initState = GSFrozen $ wrapPath Nothing (reversePath input)
  -- prepare the input: eval slices, wrap in Inner, add Start/Stop
  wrapPath :: Maybe tr' -> Path slc' tr' -> Path (Maybe tr') slc
  wrapPath eleft (PathEnd a) = Path eleft (evalSlice eval a) $ PathEnd Nothing
  wrapPath eleft (Path a e rst) =
    Path eleft (evalSlice eval a) $ wrapPath (Just e) rst

  -- parsing loop
  parse
    :: GreedyState tr tr' slc (Leftmost s f h)
    -> ExceptT String m (tr, [Leftmost s f h])

  -- case 1: everything frozen
  parse state = do
    -- liftIO $ putStrLn "" >> print state
    case state of
      GSFrozen frozen -> case frozen of
        -- only one transition: unfreeze and terminate
        PathEnd trans -> do
          (Trans thawed _, op) <-
            pickSingle $
              collectThawSingle Start trans Stop
          pure (thawed, [LMSingle op])
        -- several transition: unfreeze last and continue
        Path t slice rst -> do
          (thawed, op) <- pickSingle $ collectThawSingle (Inner slice) t Stop
          parse $ GSSemiOpen rst slice (PathEnd thawed) [LMSingle op]

      -- case 2: everything open
      GSOpen open ops -> case open of
        -- only one transition: terminate
        PathEnd (Trans t _) -> pure (t, ops)
        -- two transitions: unsplit single and terminate
        Path tl slice (PathEnd tr) -> do
          (Trans ttop _, optop) <-
            pickSingle $
              collectUnsplitSingle Start tl slice tr Stop
          pure (ttop, LMSingle optop : ops)
        -- more than two transitions: pick double operation and continue
        Path tl sl (Path tm sr rst) -> do
          let doubles = collectDoubles Start tl sl tm sr rst
          ((topl, tops, topr), op) <- pickDouble doubles
          parse $
            GSOpen
              (Path topl tops (pathSetHead rst topr))
              (LMDouble op : ops)

      -- case 3: some parts frozen, some open
      GSSemiOpen frozen mid open ops -> case open of
        -- only one open transition: thaw
        PathEnd topen -> case frozen of
          PathEnd tfrozen -> do
            ((thawed, _, _), op) <-
              pickDouble $
                collectThawLeft Start tfrozen mid topen Stop
            parse $ GSOpen (Path thawed mid open) (LMDouble op : ops)
          Path tfrozen sfrozen rstFrozen -> do
            ((thawed, _, _), op) <-
              pickDouble $
                collectThawLeft (Inner sfrozen) tfrozen mid topen Stop
            parse $
              GSSemiOpen
                rstFrozen
                sfrozen
                (Path thawed mid open)
                (LMDouble op : ops)
        -- two open transitions: thaw or unsplit single
        Path topenl sopen (PathEnd topenr) -> do
          let
            unsplits =
              Left <$> collectUnsplitSingle (Inner mid) topenl sopen topenr Stop
          case frozen of
            PathEnd tfrozen -> do
              let
                thaws =
                  Right
                    <$> collectThawLeft Start tfrozen mid topenl (Inner sopen)
              action <- pick $ thaws <> unsplits
              case action of
                -- picked unsplit
                Left (ActionSingle (_, parent, _) op) ->
                  parse $
                    GSSemiOpen
                      frozen
                      mid
                      (PathEnd parent)
                      (LMSingle op : ops)
                -- picked thaw
                Right (ActionDouble (_, thawed, _, _, _) op) ->
                  parse $ GSOpen (Path thawed mid open) (LMDouble op : ops)
            Path tfrozen sfrozen rstFrozen -> do
              let thaws =
                    Right
                      <$> collectThawLeft
                        (Inner sfrozen)
                        tfrozen
                        mid
                        topenl
                        (Inner sopen)
              action <- pick $ thaws <> unsplits
              case action of
                -- picked unsplit
                Left (ActionSingle (_, parent, _) op) ->
                  parse $
                    GSSemiOpen
                      frozen
                      mid
                      (PathEnd parent)
                      (LMSingle op : ops)
                -- picked thaw
                Right (ActionDouble (_, thawed, _, _, _) op) ->
                  parse $
                    GSSemiOpen
                      rstFrozen
                      sfrozen
                      (Path thawed mid open)
                      (LMDouble op : ops)
        -- more than two open transitions: thaw or any double operation
        Path topenl sopenl (Path topenm sopenr rstOpen) -> do
          let doubles =
                collectDoubles (Inner mid) topenl sopenl topenm sopenr rstOpen
          case frozen of
            PathEnd tfrozen -> do
              let thaws =
                    collectThawLeft Start tfrozen mid topenl (Inner sopenl)
              action <- pickDouble $ thaws <> doubles
              case action of
                -- picked thaw
                ((thawed, _, _), op@(LMDoubleFreezeLeft _)) ->
                  parse $ GSOpen (Path thawed mid open) (LMDouble op : ops)
                -- picked non-thaw
                ((topl, tops, topr), op) ->
                  parse $
                    GSSemiOpen
                      frozen
                      mid
                      (Path topl tops (pathSetHead rstOpen topr))
                      (LMDouble op : ops)
            Path tfrozen sfrozen rstFrozen -> do
              let
                thaws =
                  collectThawLeft
                    (Inner sfrozen)
                    tfrozen
                    mid
                    topenl
                    (Inner sopenl)
              action <- pickDouble $ thaws <> doubles
              case action of
                -- picked thaw
                ((thawed, _, _), op@(LMDoubleFreezeLeft _)) ->
                  parse $
                    GSSemiOpen
                      rstFrozen
                      sfrozen
                      (Path thawed mid open)
                      (LMDouble op : ops)
                -- picked non-thaw
                ((topl, tops, topr), op) ->
                  parse $
                    GSSemiOpen
                      frozen
                      mid
                      (Path topl tops (pathSetHead rstOpen topr))
                      (LMDouble op : ops)

  pickSingle
    :: [ActionSingle slc tr s f] -> ExceptT String m (Trans tr, LeftmostSingle s f)
  pickSingle actions = do
    -- liftIO $ putStrLn $ "pickSingle " <> show actions
    action <- pick $ Left <$> actions
    case action of
      Left (ActionSingle (_, top, _) op) -> pure (top, op)
      Right _ -> throwError "pickSingle returned a double action"

  pickDouble
    :: [ActionDouble slc tr s f h]
    -> ExceptT String m ((Trans tr, slc, Trans tr), LeftmostDouble s f h)
  pickDouble actions = do
    -- liftIO $ putStrLn $ "pickDouble " <> show actions
    action <- pick $ Right <$> actions
    case action of
      Left _ -> throwError "pickDouble returned a single action"
      Right (ActionDouble (_, topl, tops, topr, _) op) ->
        pure ((topl, tops, topr), op)

  collectThawSingle
    :: (StartStop slc -> Maybe tr' -> StartStop slc -> [ActionSingle slc tr s f])
  collectThawSingle sl t sr =
    mapMaybe
      getAction
      (evalUnfreeze eval sl t sr True)
   where
    getAction (t', op) = case op of
      LMSingle sop -> Just $ ActionSingle (sl, Trans t' False, sr) sop
      LMDouble _ -> Nothing

  collectThawLeft
    :: ( StartStop slc
         -> Maybe tr'
         -> slc
         -> Trans tr
         -> StartStop slc
         -> [ActionDouble slc tr s f h]
       )
  collectThawLeft sl tl sm (Trans tr _) sr =
    mapMaybe
      getAction
      (evalUnfreeze eval sl tl (Inner sm) False)
   where
    getAction (thawed, op) = case op of
      LMDouble dop ->
        Just $ ActionDouble (sl, Trans thawed False, sm, Trans tr False, sr) dop
      LMSingle _ -> Nothing

  collectUnsplitSingle
    :: ( StartStop slc
         -> Trans tr
         -> slc
         -> Trans tr
         -> StartStop slc
         -> [ActionSingle slc tr s f]
       )
  collectUnsplitSingle sl (Trans tl _) sm (Trans tr _) sr =
    mapMaybe getAction $ evalUnsplit eval sl tl sm tr sr SingleOfOne
   where
    getAction (ttop, op) = case op of
      LMSingle sop -> Just $ ActionSingle (sl, Trans ttop False, sr) sop
      LMDouble _ -> Nothing

  collectUnsplitLeft
    :: ( StartStop slc
         -> Trans tr
         -> slc
         -> Trans tr
         -> slc
         -> Trans tr
         -> StartStop slc
         -> [ActionDouble slc tr s f h]
       )
  collectUnsplitLeft sstart (Trans tl _) sl (Trans tm _) sr (Trans tr _) send =
    mapMaybe getAction $ evalUnsplit eval sstart tl sl tm (Inner sr) LeftOfTwo
   where
    getAction (ttop, op) = case op of
      LMSingle _ -> Nothing
      LMDouble dop ->
        Just $
          ActionDouble
            (sstart, Trans ttop False, sr, Trans tr False, send)
            dop

  collectUnsplitRight
    :: ( StartStop slc
         -> Trans tr
         -> slc
         -> Trans tr
         -> slc
         -> Trans tr
         -> StartStop slc
         -> [ActionDouble slc tr s f h]
       )
  collectUnsplitRight sstart tl sl (Trans tm m2nd) sr (Trans tr _) send
    | not m2nd = []
    | otherwise =
        mapMaybe getAction $
          evalUnsplit eval (Inner sl) tm sr tr send RightOfTwo
   where
    getAction (ttop, op) = case op of
      LMSingle _ -> Nothing
      LMDouble dop ->
        Just $ ActionDouble (sstart, tl, sl, Trans ttop True, send) dop

  collectUnspreads
    :: ( StartStop slc
         -> Trans tr
         -> slc
         -> Trans tr
         -> slc
         -> Trans tr
         -> StartStop slc
         -> [ActionDouble slc tr s f h]
       )
  collectUnspreads sstart (Trans tl _) sl (Trans tm _) sr (Trans tr _) send =
    catMaybes $ do
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
            (sstart, Trans lTop False, sTop, Trans rTop True, send)
            dop

  collectDoubles sstart tl sl tm sr rst = leftUnsplits <> rightUnsplits <> unspreads
   where
    (tr, send) = case rst of
      PathEnd t -> (t, Stop)
      Path t s _ -> (t, Inner s)
    leftUnsplits = collectUnsplitLeft sstart tl sl tm sr tr send
    rightUnsplits = collectUnsplitRight sstart tl sl tm sr tr send
    unspreads = collectUnspreads sstart tl sl tm sr tr send

{- | A policy that picks the next action at random.
 Must be partially applied with a random generator before passing to 'parseGreedy'.
-}
pickRandom :: StatefulGen g m => g -> [slc] -> ExceptT String m slc
pickRandom _ [] = throwError "No candidates for pickRandom!"
pickRandom gen xs = do
  i <- lift $ uniformRM (0, length xs - 1) gen
  pure $ xs !! i

-- * Entry Points

-- | Parse a piece randomly using a fresh random number generator.
parseRandom
  :: (Show tr', Show slc, Show tr, Show s, Show f, Show h)
  => Eval tr tr' slc slc' (Leftmost s f h)
  -- ^ the grammar's evaluator
  -> Path slc' tr'
  -- ^ the input piece
  -> ExceptT String IO (Analysis s f h tr slc)
  -- ^ a random reduction of the piece (or an error message)
parseRandom eval input = do
  gen <- lift initStdGen
  mgen <- lift $ newIOGenM gen
  parseGreedy eval (pickRandom mgen) input

-- | Parse a piece randomly using an existing random number generator.
parseRandom'
  :: (Show tr', Show slc, Show tr, Show s, Show f, Show h, StatefulGen g IO)
  => g
  -- ^ a random number generator
  -> Eval tr tr' slc slc' (Leftmost s f h)
  -- ^ the grammar's evaluator
  -> Path slc' tr'
  -- ^ the input piece
  -> ExceptT String IO (Analysis s f h tr slc)
  -- ^ a random reduction of the piece (or an error message)
parseRandom' mgen eval input = do
  parseGreedy eval (pickRandom mgen) input
