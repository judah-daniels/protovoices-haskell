{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Common

import           Control.Monad.Except           ( ExceptT
                                                , MonadError(throwError)
                                                , runExceptT
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
import           Control.Monad.Except           ( ExceptT
                                                , MonadError(throwError)
                                                ) 

import qualified Data.Vector as V
import Display
import GHC.IO.Handle.Types (Handle (FileHandle))
import Language.Haskell.DoNotation
import Musicology.Core
import qualified Musicology.Core as Music
import Musicology.Core.Slicing
import qualified Musicology.Core.Slicing as Music
import Musicology.MusicXML
import qualified Musicology.MusicXML as MusicXML
import Musicology.Pitch.Spelled as MT

import PVGrammar hiding
  ( slicesFromFile,
  )
import PVGrammar.Generate
import PVGrammar.Parse
-- import Parser
import qualified Scoring.FunTyped as S
import System.FilePattern (step)
import Prelude hiding
  ( Monad (..),
    pure,
  )

-- | The musical surface from Figure 4 as a sequence of slices and transitions.
-- Can be used as an input for parsing.
path321sus =
  Path [e' nat, c' nat] [(Inner $ c' nat, Inner $ c' nat)]
    $ Path [d' nat, c' nat] [(Inner $ d' nat, Inner $ d' nat)]
    $ Path [d' nat, b' nat] []
    $ PathEnd [c' nat]

main :: IO ()
main = mainParseStep

mainParseStep :: IO ()
mainParseStep = do
  putStrLn "Input:"
  print path321sus
  putStrLn "Attempting to perform a single parse step"
  result <- runExceptT $ parseRandom protoVoiceEvaluator path321sus
  print result
  case result of
    Left err -> print err
    Right (Analysis a topslice) -> plotDeriv "321sus-parsed.tex" a 

searchStep
  :: forall m e e' a a' s f h
   . (Monad m, MonadIO m, Show e', Show a, Show e, Show s, Show f, Show h)
  => Eval e e' a a' (Leftmost s f h)
  -> ([Action a e s f h] -> ExceptT String m (Action a e s f h))
  -> Path a' e'
  -> ExceptT String m (Analysis s f h e a) -- (e, [Leftmost s f h])
searchStep eval pick input = do
  (top, deriv) <- parse initState
  pure $ Analysis deriv $ PathEnd top
  where
    initState = SSFrozen $ wrapPath Nothing (reversePath input)
    -- prepare the input: eval slices, wrap in Inner, add Start/Stop
    wrapPath :: Maybe e' -> Path a' e' -> Path (Maybe e') a
    wrapPath eleft (PathEnd a) = Path eleft (evalSlice eval a) $ PathEnd Nothing
    wrapPath eleft (Path a e rst) =
      Path eleft (evalSlice eval a) $ wrapPath (Just e) rst

    -- parsing loop
    parse
      :: SearchState e e' a (Leftmost s f h)
      -> ExceptT String m (e, [Leftmost s f h])
    parse state = case state of
      SSFrozen frozen -> case frozen of
        -- only one transition: unfreeze and terminate
        PathEnd trans -> do
          (thawed, op) <- pickSingle
            $ collectThawSingle Start trans Stop
          pure (thawed, [LMSingle op])

        -- -- several transition: unfreeze last and continue
        -- Path t slice rst -> do
        --   (thawed, op) <- pickSingle $ collectThawSingle (Inner slice) t Stop
        --   pure (thawed, [LMSingle op])
        --
        -- several transition: unfreeze last and continue
        Path t slice rst -> do
          (thawed, op) <- pickSingle $ collectThawSingle (Inner slice) t Stop
          parse $ SSSemiOpen rst slice (PathEnd thawed) [LMSingle op]


      -- case 2: everything open
      SSOpen open ops -> case open of
        -- only one transition: terminate
        PathEnd t        -> pure (t, ops)
        -- two transitions: merge single and terminate
        Path tl slice (PathEnd tr) -> do
          (ttop, optop) <- pickSingle
            $ collectMergeSingle Start tl slice tr Stop
          pure (ttop, LMSingle optop : ops)
        -- more than two transitions: pick double operation and continue
        Path tl sl (Path tm sr rst) -> do
          let doubles = collectDoubles Start tl sl tm sr rst
          ((topl, tops, topr), op) <- pickDouble doubles
          parse $ SSOpen (Path topl tops (pathSetHead rst topr))
                         (LMDouble op : ops)

      -- case 3: some parts frozen, some open
      SSSemiOpen frozen mid open ops -> case open of
        -- only one open transition: thaw
        PathEnd topen -> case frozen of
          PathEnd tfrozen -> do
            ((thawed, _, _), op) <- pickDouble
              $ collectThawLeft Start tfrozen mid topen Stop
            parse $ SSOpen (Path thawed mid open) (LMDouble op : ops)
          Path tfrozen sfrozen rstFrozen -> do
            ((thawed, _, _), op) <- pickDouble
              $ collectThawLeft (Inner sfrozen) tfrozen mid topen Stop
            parse $ SSSemiOpen rstFrozen
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
                Left (ActionSingle (_, merged, _) op) -> parse $ SSSemiOpen
                  frozen
                  mid
                  (PathEnd merged)
                  (LMSingle op : ops)
                -- picked thaw
                Right (ActionDouble (_, thawed, _, _, _) op) ->
                  parse $ SSOpen (Path thawed mid open) (LMDouble op : ops)
            Path tfrozen sfrozen rstFrozen -> do
              let thaws = Right <$> collectThawLeft (Inner sfrozen)
                                                    tfrozen
                                                    mid
                                                    topenl
                                                    (Inner sopen)
              action <- pick $ thaws <> merges
              case action of
                -- picked merge
                Left (ActionSingle (_, merged, _) op) -> parse $ SSSemiOpen
                  frozen
                  mid
                  (PathEnd merged)
                  (LMSingle op : ops)
                -- picked thaw
                Right (ActionDouble (_, thawed, _, _, _) op) ->
                  parse $ SSSemiOpen rstFrozen
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
                  parse $ SSOpen (Path thawed mid open) (LMDouble op : ops)
                -- picked non-thaw
                ((topl, tops, topr), op) -> parse $ SSSemiOpen
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
                  parse $ SSSemiOpen rstFrozen
                                     sfrozen
                                     (Path thawed mid open)
                                     (LMDouble op : ops)
                -- picked non-thaw
                ((topl, tops, topr), op) -> parse $ SSSemiOpen
                  frozen
                  mid
                  (Path topl tops (pathSetHead rstOpen topr))
                  (LMDouble op : ops)

    pickSingle
      :: [ActionSingle a e s f] -> ExceptT String m (e, LeftmostSingle s f)
    pickSingle actions = do
      -- liftIO $ putStrLn $ "pickSingle " <> show actions
      action <- pick $ Left <$> actions
      case action of
        Left (ActionSingle (_, top, _) op) -> pure (top, op)
        Right _ -> throwError "pickSingle returned a double action"

    pickDouble
      :: [ActionDouble a e s f h]
      -> ExceptT String m ((e, a, e), LeftmostDouble s f h)
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
        LMSingle sop -> Just $ ActionSingle (sl, t', sr) sop
        LMDouble _   -> Nothing

    collectThawLeft
      :: (  StartStop a
         -> Maybe e'
         -> a
         -> e
         -> StartStop a
         -> [ActionDouble a e s f h]
         )
    collectThawLeft sl tl sm tr sr = mapMaybe
      getAction
      (evalThaw eval sl tl (Inner sm) False)
     where
      getAction (thawed, op) = case op of
        LMDouble dop ->
          Just $ ActionDouble (sl, thawed , sm, tr , sr) dop
        LMSingle _ -> Nothing

    collectMergeSingle
      :: (  StartStop a
         -> e
         -> a
         -> e
         -> StartStop a
         -> [ActionSingle a e s f]
         )
    collectMergeSingle sl tl sm tr sr =
      mapMaybe getAction $ evalMerge eval sl tl sm tr sr LeftOnly
     where
      getAction (ttop, op) = case op of
        LMSingle sop -> Just $ ActionSingle (sl,ttop, sr) sop
        LMDouble _   -> Nothing

    collectMergeLeft
      :: (  StartStop a
         -> e
         -> a
         -> e
         -> a
         -> e
         -> StartStop a
         -> [ActionDouble a e s f h]
         )
    collectMergeLeft sstart tl sl tm sr tr send =
      mapMaybe getAction $ evalMerge eval sstart tl sl tm (Inner sr) LeftOfTwo
     where
      getAction (ttop, op) = case op of
        LMSingle _   -> Nothing
        LMDouble dop -> Just $ ActionDouble
          (sstart, ttop , sr, tr , send)
          dop

    collectMergeRight
      :: (  StartStop a
         -> e
         -> a
         -> e
         -> a
         -> e
         -> StartStop a
         -> [ActionDouble a e s f h]
         )
    collectMergeRight sstart tl sl tm sr tr send = 
      mapMaybe getAction
      $ evalMerge eval (Inner sl) tm sr tr send RightOfTwo
     where
      getAction (ttop, op) = case op of
        LMSingle _ -> Nothing
        LMDouble dop ->
          Just $ ActionDouble (sstart, tl, sl, ttop, send) dop

    collectVerts
      :: (  StartStop a
         -> e
         -> a
         -> e
         -> a
         -> e
         -> StartStop a
         -> [ActionDouble a e s f h]
         )
    collectVerts sstart tl sl tm sr tr send =
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
          (sstart, lTop, sTop, rTop, send)
          dop

    collectDoubles sstart tl sl tm sr rst = leftMerges <> rightMerges <> verts
     where
      (tr, send) = case rst of
        PathEnd t  -> (t, Stop)
        Path t s _ -> (t, Inner s)
      leftMerges  = collectMergeLeft sstart tl sl tm sr tr send
      rightMerges = collectMergeRight sstart tl sl tm sr tr send
      verts       = collectVerts sstart tl sl tm sr tr send


pickRandom :: Monad m => [a] -> ExceptT String m a
pickRandom [] = throwError "No candidates for pickRandom!"
pickRandom xs = do
  pure $ last xs 

parseRandom
  :: (Show e', Show a, Show e, Show s, Show f, Show h)
  => Eval e e' a a' (Leftmost s f h)
  -> Path a' e'
  -> ExceptT String IO (Analysis s f h e a)
parseRandom eval input = do
  searchStep eval pickRandom input


-- | The state of the search between steps
data SearchState e e' a o = SSFrozen (Path (Maybe e') a)
                          | SSSemiOpen
                            { _ssFrozen   :: Path (Maybe e') a
                              -- ^ frozen transitions and slices from current point leftward
                            , _ssMidSlice :: a
                              -- ^ the slice at the current posision between gsFrozen and gsOpen
                            , _ssOpen     :: Path e a
                              -- ^ non-frozen transitions and slices from current point rightward
                            , _ssDeriv :: [o]
                              -- ^ derivation from current reduction to original surface
                            }
                          | SSOpen (Path e a) [o]


instance (Show a, Show o) => Show (SearchState e e' a o) where
  show (SSFrozen frozen ) = showFrozen frozen <> "⋉"
  show (SSOpen open _ops) = "⋊" <> showOpen open -- <> " " <> show ops
  show (SSSemiOpen frozen mid open _ops) =
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

-- Actions
type Action a e s f h = Either (ActionSingle a e s f) (ActionDouble a e s f h)

data ActionSingle a e s f = ActionSingle (StartStop a, e, StartStop a)
                                         (LeftmostSingle s f)
  deriving Show
  
data ActionDouble a e s f h = ActionDouble ( StartStop a
                                           , e
                                           , a
                                           , e
                                           , StartStop a
                                           )
                                           (LeftmostDouble s f h)
  deriving Show


plotDeriv fn deriv = do
  case replayDerivation derivationPlayerPV deriv of
    (Left  err) -> putStrLn err
    (Right g  ) -> viewGraph fn g

