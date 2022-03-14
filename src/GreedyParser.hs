{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module GreedyParser
  ( parseGreedy
  , pickRandom
  , parseRandom
  , parseRandom'
  ) where

import           Common

import           Control.Monad.Except           ( ExceptT
                                                , MonadError(throwError)
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                --, liftIO
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Data.Maybe                     ( catMaybes
                                                , mapMaybe
                                                , maybeToList
                                                )
import           System.Random                  ( initStdGen )
import           System.Random.Stateful         ( StatefulGen
                                                , newIOGenM
                                                , uniformRM
                                                )

data Trans e = Trans
  { _tContent :: e
  , _t2nd     :: Bool
  }
  deriving Show

-- | The state of the greedy parse between steps
data GreedyState e e' a o = GSFrozen (Path (Maybe e') a)
                          | GSSemiOpen
                            { _gsFrozen   :: Path (Maybe e') a
                              -- ^ frozen transitions and slices from current point leftward
                            , _gsMidSlice :: a
                              -- ^ the slice at the current posision between gsFrozen and gsOpen
                            , _gsOpen     :: Path (Trans e) a
                              -- ^ non-frozen transitions and slices from current point rightward
                            , _gsDeriv :: [o]
                              -- ^ derivation from current reduction to original surface
                            }
                          | GSOpen (Path (Trans e) a) [o]

instance (Show a, Show o) => Show (GreedyState e e' a o) where
  show (GSFrozen frozen ) = showFrozen frozen <> "⋉"
  show (GSOpen open _ops) = "⋊" <> showOpen open -- <> " " <> show ops
  show (GSSemiOpen frozen mid open _ops) =
    showFrozen frozen <> show mid <> showOpen open -- <> " " <> show ops

showFrozen :: Show a => Path e a -> [Char]
showFrozen path = "⋊" <> go path
 where
  go (PathEnd _   ) = "="
  go (Path _ a rst) = go rst <> show a <> "="

showOpen :: Show a => Path e a -> [Char]
showOpen path = go path <> "⋉"
 where
  go (PathEnd _   ) = "-"
  go (Path _ a rst) = "-" <> show a <> go rst

data ActionSingle a e s f = ActionSingle (StartStop a, Trans e, StartStop a)
                                         (LeftmostSingle s f)
  deriving Show
data ActionDouble a e s f h = ActionDouble ( StartStop a
                                           , Trans e
                                           , a
                                           , Trans e
                                           , StartStop a
                                           )
                                           (LeftmostDouble s f h)
  deriving Show
type Action a e s f h = Either (ActionSingle a e s f) (ActionDouble a e s f h)

parseGreedy
  :: forall m e e' a a' s f h
   . (Monad m, MonadIO m, Show e', Show a, Show e, Show s, Show f, Show h)
  => Eval e e' a a' (Leftmost s f h)
  -> ([Action a e s f h] -> ExceptT String m (Action a e s f h))
  -> Path a' e'
  -> ExceptT String m (Analysis s f h e a) -- (e, [Leftmost s f h])
parseGreedy eval pick input = do
  (top, deriv) <- parse initState
  pure $ Analysis deriv $ PathEnd top
 where
  initState = GSFrozen $ wrapPath Nothing (reversePath input)
  -- prepare the input: eval slices, wrap in Inner, add Start/Stop
  wrapPath :: Maybe e' -> Path a' e' -> Path (Maybe e') a
  wrapPath eleft (PathEnd a) = Path eleft (evalSlice eval a) $ PathEnd Nothing
  wrapPath eleft (Path a e rst) =
    Path eleft (evalSlice eval a) $ wrapPath (Just e) rst

  -- parsing loop
  parse
    :: GreedyState e e' a (Leftmost s f h)
    -> ExceptT String m (e, [Leftmost s f h])

  -- case 1: everything frozen
  parse state = do
    -- liftIO $ putStrLn "" >> print state
    case state of
      GSFrozen frozen -> case frozen of
        -- only one transition: unfreeze and terminate
        PathEnd trans -> do
          (Trans thawed _, op) <- pickSingle
            $ collectThawSingle Start trans Stop
          pure (thawed, [LMSingle op])
        -- several transition: unfreeze last and continue
        Path t slice rst -> do
          (thawed, op) <- pickSingle $ collectThawSingle (Inner slice) t Stop
          parse $ GSSemiOpen rst slice (PathEnd thawed) [LMSingle op]

      -- case 2: everything open
      GSOpen open ops -> case open of
        -- only one transition: terminate
        PathEnd (Trans t _)        -> pure (t, ops)
        -- two transitions: merge single and terminate
        Path tl slice (PathEnd tr) -> do
          (Trans ttop _, optop) <- pickSingle
            $ collectMergeSingle Start tl slice tr Stop
          pure (ttop, LMSingle optop : ops)
        -- more than two transitions: pick double operation and continue
        Path tl sl (Path tm sr rst) -> do
          let doubles = collectDoubles Start tl sl tm sr rst
          ((topl, tops, topr), op) <- pickDouble doubles
          parse $ GSOpen (Path topl tops (pathSetHead rst topr))
                         (LMDouble op : ops)

      -- case 3: some parts frozen, some open
      GSSemiOpen frozen mid open ops -> case open of
        -- only one open transition: thaw
        PathEnd topen -> case frozen of
          PathEnd tfrozen -> do
            ((thawed, _, _), op) <- pickDouble
              $ collectThawLeft Start tfrozen mid topen Stop
            parse $ GSOpen (Path thawed mid open) (LMDouble op : ops)
          Path tfrozen sfrozen rstFrozen -> do
            ((thawed, _, _), op) <- pickDouble
              $ collectThawLeft (Inner sfrozen) tfrozen mid topen Stop
            parse $ GSSemiOpen rstFrozen
                               sfrozen
                               (Path thawed mid open)
                               (LMDouble op : ops)
        -- two open transitions: thaw or merge single
        Path topenl sopen (PathEnd topenr) -> do
          let
            merges =
              Left <$> collectMergeSingle (Inner mid) topenl sopen topenr Stop
          case frozen of
            PathEnd tfrozen -> do
              let
                thaws = Right
                  <$> collectThawLeft Start tfrozen mid topenl (Inner sopen)
              action <- pick $ thaws <> merges
              case action of
                -- picked merge
                Left (ActionSingle (_, merged, _) op) -> parse $ GSSemiOpen
                  frozen
                  mid
                  (PathEnd merged)
                  (LMSingle op : ops)
                -- picked thaw
                Right (ActionDouble (_, thawed, _, _, _) op) ->
                  parse $ GSOpen (Path thawed mid open) (LMDouble op : ops)
            Path tfrozen sfrozen rstFrozen -> do
              let thaws = Right <$> collectThawLeft (Inner sfrozen)
                                                    tfrozen
                                                    mid
                                                    topenl
                                                    (Inner sopen)
              action <- pick $ thaws <> merges
              case action of
                -- picked merge
                Left (ActionSingle (_, merged, _) op) -> parse $ GSSemiOpen
                  frozen
                  mid
                  (PathEnd merged)
                  (LMSingle op : ops)
                -- picked thaw
                Right (ActionDouble (_, thawed, _, _, _) op) ->
                  parse $ GSSemiOpen rstFrozen
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
                ((topl, tops, topr), op) -> parse $ GSSemiOpen
                  frozen
                  mid
                  (Path topl tops (pathSetHead rstOpen topr))
                  (LMDouble op : ops)
            Path tfrozen sfrozen rstFrozen -> do
              let
                thaws = collectThawLeft (Inner sfrozen)
                                        tfrozen
                                        mid
                                        topenl
                                        (Inner sopenl)
              action <- pickDouble $ thaws <> doubles
              case action of
                -- picked thaw
                ((thawed, _, _), op@(LMDoubleFreezeLeft _)) ->
                  parse $ GSSemiOpen rstFrozen
                                     sfrozen
                                     (Path thawed mid open)
                                     (LMDouble op : ops)
                -- picked non-thaw
                ((topl, tops, topr), op) -> parse $ GSSemiOpen
                  frozen
                  mid
                  (Path topl tops (pathSetHead rstOpen topr))
                  (LMDouble op : ops)

  pickSingle
    :: [ActionSingle a e s f] -> ExceptT String m (Trans e, LeftmostSingle s f)
  pickSingle actions = do
    -- liftIO $ putStrLn $ "pickSingle " <> show actions
    action <- pick $ Left <$> actions
    case action of
      Left (ActionSingle (_, top, _) op) -> pure (top, op)
      Right _ -> throwError "pickSingle returned a double action"

  pickDouble
    :: [ActionDouble a e s f h]
    -> ExceptT String m ((Trans e, a, Trans e), LeftmostDouble s f h)
  pickDouble actions = do
    -- liftIO $ putStrLn $ "pickDouble " <> show actions
    action <- pick $ Right <$> actions
    case action of
      Left _ -> throwError "pickDouble returned a single action"
      Right (ActionDouble (_, topl, tops, topr, _) op) ->
        pure ((topl, tops, topr), op)

  collectThawSingle
    :: (StartStop a -> Maybe e' -> StartStop a -> [ActionSingle a e s f])
  collectThawSingle sl t sr = mapMaybe getAction (evalThaw eval sl t sr True)
   where
    getAction (t', op) = case op of
      LMSingle sop -> Just $ ActionSingle (sl, Trans t' False, sr) sop
      LMDouble _   -> Nothing

  collectThawLeft
    :: (  StartStop a
       -> Maybe e'
       -> a
       -> Trans e
       -> StartStop a
       -> [ActionDouble a e s f h]
       )
  collectThawLeft sl tl sm (Trans tr _) sr = mapMaybe
    getAction
    (evalThaw eval sl tl (Inner sm) False)
   where
    getAction (thawed, op) = case op of
      LMDouble dop ->
        Just $ ActionDouble (sl, Trans thawed False, sm, Trans tr False, sr) dop
      LMSingle _ -> Nothing

  collectMergeSingle
    :: (  StartStop a
       -> Trans e
       -> a
       -> Trans e
       -> StartStop a
       -> [ActionSingle a e s f]
       )
  collectMergeSingle sl (Trans tl _) sm (Trans tr _) sr =
    mapMaybe getAction $ evalMerge eval sl tl sm tr sr LeftOnly
   where
    getAction (ttop, op) = case op of
      LMSingle sop -> Just $ ActionSingle (sl, Trans ttop False, sr) sop
      LMDouble _   -> Nothing

  collectMergeLeft
    :: (  StartStop a
       -> Trans e
       -> a
       -> Trans e
       -> a
       -> Trans e
       -> StartStop a
       -> [ActionDouble a e s f h]
       )
  collectMergeLeft sstart (Trans tl _) sl (Trans tm _) sr (Trans tr _) send =
    mapMaybe getAction $ evalMerge eval sstart tl sl tm (Inner sr) LeftOfTwo
   where
    getAction (ttop, op) = case op of
      LMSingle _   -> Nothing
      LMDouble dop -> Just $ ActionDouble
        (sstart, Trans ttop False, sr, Trans tr False, send)
        dop

  collectMergeRight
    :: (  StartStop a
       -> Trans e
       -> a
       -> Trans e
       -> a
       -> Trans e
       -> StartStop a
       -> [ActionDouble a e s f h]
       )
  collectMergeRight sstart tl sl (Trans tm m2nd) sr (Trans tr _) send
    | not m2nd = []
    | otherwise = mapMaybe getAction
    $ evalMerge eval (Inner sl) tm sr tr send RightOfTwo
   where
    getAction (ttop, op) = case op of
      LMSingle _ -> Nothing
      LMDouble dop ->
        Just $ ActionDouble (sstart, tl, sl, Trans ttop True, send) dop

  collectVerts
    :: (  StartStop a
       -> Trans e
       -> a
       -> Trans e
       -> a
       -> Trans e
       -> StartStop a
       -> [ActionDouble a e s f h]
       )
  collectVerts sstart (Trans tl _) sl (Trans tm _) sr (Trans tr _) send =
    catMaybes $ do -- List
      (sTop, op) <- maybeToList $ evalVertMiddle eval (sl, tm, sr)
      lTop       <- evalVertLeft eval (tl, sl) sTop
      rTop       <- evalVertRight eval (sr, tr) sTop
      pure $ getAction lTop sTop rTop op
      -- pure $ getAction $ evalMerge eval (Inner sl) tm sr tr send RightOfTwo
   where
    getAction lTop sTop rTop op = case op of
      LMSingle _   -> Nothing
      LMDouble dop -> Just $ ActionDouble
        (sstart, Trans lTop False, sTop, Trans rTop True, send)
        dop

  collectDoubles sstart tl sl tm sr rst = leftMerges <> rightMerges <> verts
   where
    (tr, send) = case rst of
      PathEnd t  -> (t, Stop)
      Path t s _ -> (t, Inner s)
    leftMerges  = collectMergeLeft sstart tl sl tm sr tr send
    rightMerges = collectMergeRight sstart tl sl tm sr tr send
    verts       = collectVerts sstart tl sl tm sr tr send

pickRandom :: StatefulGen g m => g -> [a] -> ExceptT String m a
pickRandom _   [] = throwError "No candidates for pickRandom!"
pickRandom gen xs = do
  i <- lift $ uniformRM (0, length xs - 1) gen
  pure $ xs !! i

parseRandom
  :: (Show e', Show a, Show e, Show s, Show f, Show h)
  => Eval e e' a a' (Leftmost s f h)
  -> Path a' e'
  -> ExceptT String IO (Analysis s f h e a)
parseRandom eval input = do
  gen  <- lift initStdGen
  mgen <- lift $ newIOGenM gen
  parseGreedy eval (pickRandom mgen) input

parseRandom'
  :: (Show e', Show a, Show e, Show s, Show f, Show h, StatefulGen g IO)
  => g
  -> Eval e e' a a' (Leftmost s f h)
  -> Path a' e'
  -> ExceptT String IO (Analysis s f h e a)
parseRandom' mgen eval input = do
  parseGreedy eval (pickRandom mgen) input


-- data EitherTag = EitherTagLeft | EitherTagRight | EitherTagBoth

-- type family EitherTagPlus (t1 :: EitherTag) (t2 :: EitherTag) where
--   EitherTagPlus 'EitherTagLeft 'EitherTagLeft = 'EitherTagLeft
--   EitherTagPlus 'EitherTagRight 'EitherTagRight = 'EitherTagRight
--   EitherTagPlus a b = 'EitherTagBoth

-- data EitherList (t :: EitherTag) a b where
--   ELNil ::EitherList t a b
--   ELLeft ::a -> EitherList t a b -> EitherList (EitherTagPlus t 'EitherTagLeft) a b
--   ELRight ::b -> EitherList t a b -> EitherList (EitherTagPlus t 'EitherTagRight) a b

-- listToLefts :: [a] -> EitherList 'EitherTagLeft a b
-- listToLefts = foldr ELLeft ELNil

-- listToRights :: [b] -> EitherList 'EitherTagRight a b
-- listToRights = foldr ELRight ELNil

-- listToEithers :: [Either a b] -> EitherList 'EitherTagBoth a b
-- listToEithers = foldr
--   (\case
--     Left  a -> ELLeft a
--     Right b -> ELRight b
--   )
--   ELNil
