{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-all #-}
module Main where

import           Common
import           Display
import           GreedyParser                  as Greedy
import           PVGrammar
import           PVGrammar.Generate
import           PVGrammar.Parse
import           PVGrammar.Prob.Simple          ( observeDerivation
                                                , sampleDerivation
                                                )
import           Parser
import           ScoresCommon                   ( FreezeScore(FreezeScore)
                                                , LeftmostScore
                                                , SplitScore(SplitScore)
                                                , SpreadScore(SpreadScore)
                                                )

import           Musicology.Core
import           Musicology.Core.Slicing
--import Musicology.Internal.Helpers
import           Musicology.MusicXML
import           Musicology.Pitch.Spelled      as MT

import           Data.Either                    ( partitionEithers )
import           Data.Maybe                     ( catMaybes )
import           Data.Ratio                     ( Ratio(..) )
import           Lens.Micro                     ( over )

import           Control.Monad                  ( foldM
                                                , forM
                                                , forM_
                                                )
import           Control.Monad.Except           ( runExceptT )
import qualified Data.HashSet                  as HS
import qualified Data.List                     as L
import qualified Data.Semiring                 as R
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TL
import qualified Internal.MultiSet             as MS


import           Control.DeepSeq                ( deepseq
                                                , force
                                                )
import           Control.Monad.Trans.Maybe      ( MaybeT(MaybeT) )
import           Data.Bifunctor                 ( Bifunctor(bimap) )
import           Data.String                    ( fromString )
import           Inference.Conjugate            ( runTrace
                                                , showTrace
                                                , traceTrace
                                                )
-- better do syntax
import qualified Language.Haskell.DoNotation   as Do
-- import           Prelude                 hiding ( Monad(..)
--                                                 , pure
--                                                 )
import           Prelude

ifThenElse True  t e = t
ifThenElse False t e = e

-- utilities
-- =========

-- reading files
-- -------------

testfile = "testdata/allemande.musicxml"

bb =
  "/home/chfin/dateien/dev/haskell/work/proto-voice-model/bluebossa.musicxml"

brahms1 =
  "/home/chfin/dateien/dev/haskell/work/proto-voice-model/brahms1.musicxml"

haydn5 = "/home/chfin/Uni/phd/data/kirlin_schenker/haydn5.xml"

invention =
  "/home/chfin/Uni/phd/data/protovoice-annotations/bach/inventions/BWV_0784.musicxml"

-- getPitchGroups :: FilePath -> IO [[OnOff SPitch (Ratio Int)]]
-- getPitchGroups file = do
--   txt <- TL.readFile file
--   return
--     $   fmap (fmap $ over onOffContent pitch)
--     $   onOffGroups
--     $   asNote
--     <$> xmlNotesHeard txt

testslices = loadInput' testfile

-- manual inputs
-- -------------

monopath :: [a] -> Path [a] [b]
monopath = path . fmap (: [])

path :: [a] -> Path a [b]
path []       = error "cannot construct empty path"
path [a     ] = PathEnd a
path (a : as) = Path a [] $ path as

-- actions
-- -------

printDerivs path = do
  ds <- parseSilent pvDeriv path
  forM_ (flattenDerivations ds) $ \d -> do
    putStrLn "\nDerivation:"
    forM_ d $ \step -> do
      putStrLn $ "- " <> show step
    case replayDerivation derivationPlayerPV d of
      Left  error -> putStrLn $ "Error: " <> error
      Right _     -> putStrLn "Ok."

plotDerivs fn derivs = do
  pics <- forM derivs $ \d -> case replayDerivation derivationPlayerPV d of
    Left error -> do
      putStrLn error
      print d
      return Nothing
    Right g -> return $ Just g
  viewGraphs fn $ catMaybes pics

plotDeriv fn deriv = do
  case replayDerivation derivationPlayerPV deriv of
    (Left  err) -> putStrLn err
    (Right g  ) -> viewGraph fn g

plotSteps fn deriv = do
  let graphs          = unfoldDerivation derivationPlayerPV deriv
      (errors, steps) = partitionEithers graphs
  mapM_ putStrLn errors
  viewGraphs fn $ reverse steps

checkDeriv deriv original = do
  case replayDerivation derivationPlayerPV deriv of
    (Left  err) -> putStrLn err
    (Right g  ) -> do
      let path' = case dgFoot g of
            (_ : (_, tlast, slast) : rst) -> do
              s <- getSlice slast
              foldM foldPath (PathEnd s, tlast) rst
            _ -> Nothing
          orig' = bimap (Notes . MS.fromList)
                        (\e -> Edges (HS.fromList e) MS.empty)
                        original
      case path' of
        Nothing          -> putStrLn "failed to check result path"
        Just (result, _) -> if result == orig'
          then putStrLn "roundtrip ok"
          else do
            putStrLn "roundtrip not ok, surfaces are not equal:"
            putStrLn "original:"
            print original
            putStrLn "recreated:"
            print result

 where
  foldPath (pacc, tacc) (_, tnew, snew) = do
    s <- getSlice snew
    pure (Path s tacc pacc, tnew)
  getSlice (_, _, s) = getInner s


-- example derivations
-- ===================

derivBrahms :: [PVLeftmost (Pitch MT.SIC)]
derivBrahms = buildDerivation $ do
  split $ mkSplit $ do
    splitT Start Stop (c' shp) RootNote False False
    splitT Start Stop (a' nat) RootNote False False
  spread $ mkHori $ do
    horiNote (a' nat) ToBoth     True
    horiNote (c' shp) (ToLeft 1) False
    addPassing (c' shp) (a' nat)
  splitRight $ mkSplit $ do
    splitNT (c' shp) (a' nat) (b' nat) PassingMid False False
    splitT (Inner $ a' nat) (Inner $ a' nat) (g' shp) FullNeighbor False False
  spread $ mkHori $ do
    horiNote (a' nat) (ToRight 1) False
    horiNote (c' shp) (ToLeft 1)  False
    addPassing (c' shp) (a' nat)
  freeze FreezeOp
  split $ mkSplit $ do
    splitNT (c' shp) (a' nat) (b' nat) PassingMid False False
  freeze FreezeOp
  freeze FreezeOp
  spread $ mkHori $ do
    horiNote (b' nat) (ToRight 1) False
    horiNote (g' shp) (ToLeft 1)  False
  split $ mkSplit $ do
    addToRight (g' shp) (a' nat) LeftNeighbor False
  freeze FreezeOp
  freeze FreezeOp
  split $ mkSplit $ do
    addToRight (b' nat) (c' shp) LeftNeighbor False
  freeze FreezeOp
  freeze FreezeOp
  freeze FreezeOp
  freeze FreezeOp
 where
  (>>) :: Do.BindSyntax x y z => x a -> y b -> z b
  (>>) = (Do.>>)

derivScore :: [LeftmostScore (Derivations String)]
derivScore = buildDerivation $ do
  split $ SplitScore $ Do "top"
  spread $ SpreadScore $ Do "h2"
  split $ SplitScore $ Do "s"
  freeze $ FreezeScore $ Do "L1"
  spread $ SpreadScore $ Do "h1"
  freeze $ FreezeScore $ Do "L2"
  freeze $ FreezeScore $ Do "M"
  freeze $ FreezeScore $ Do "C"
  freeze $ FreezeScore $ Do "R"
  where (>>) = (Do.>>)

-- mains
-- =====

mainGreedy file = do
  input <- loadInput file
  print input
  result <- runExceptT $ Greedy.parseRandom protoVoiceEvaluator input
  case result of
    Left  err                  -> print err
    -- Right _   -> putStrLn "Ok."
    Right (Analysis deriv top) -> do
      print "done parsing."
      checkDeriv deriv input
      case replayDerivation derivationPlayerPV deriv of
        Left  err -> putStrLn err
        Right g   -> viewGraph "greedy.tex" g
      -- case observeDerivation deriv top of
      --   Left  err   -> print err
      --   Right trace -> do
      --     print "done observing parse."
      --     putStrLn
      --       $  "trace has "
      --       <> show (Seq.length (runTrace trace))
      --       <> " items."
      --     -- let res = traceTrace trace (sampleDerivation top)
      --     -- pure ()
      forM_ deriv print



mainCount fn = do
  input <- loadInput fn
  print input
  count <- parseSize pvCount input
  putStrLn $ show count <> " derivations"

mainTest from to = do
  putStrLn $ "slices " <> show from <> " to " <> show to
  input <- testslices from (to + 1)
  print input
  count <- parseSize pvCount input
  putStrLn $ show count <> " derivations"

mainBB = do
  input <- slicesToPath <$> slicesFromFile bb
  print input
  count <- parseSize pvCount input
  print count

mainBrahms = do
  input <- slicesToPath <$> slicesFromFile brahms1
  print input
  count <- parseSize pvCount input
  print count

mainGraph = do
  input  <- slicesToPath <$> slicesFromFile brahms1
  derivs <- parseSize pvDeriv input
  let ds = S.toList $ flattenDerivations derivs
  pics <- forM ds $ \d -> case replayDerivation derivationPlayerPV d of
    Left err -> do
      putStrLn err
      print d
      return Nothing
    Right g -> return $ Just g
  print pics
  viewGraphs "brahms.tex" $ catMaybes pics

logFull tc vc n = do
  putStrLn "\n===========\n"
  putStrLn $ "level " <> show n
  putStrLn "\ntransitions:"
  mapM_ print $ tcGetByLength tc n
  putStrLn "\nverticalizations:"
  mapM_ print $ vcGetByLength vc (n - 1)

mainResult
  :: Parsable e a v
  => Eval e [Edge (Pitch SInterval)] a [Pitch SInterval] v
  -> Int
  -> Int
  -> IO v
mainResult evaluator from to = do
  putStrLn $ "slices " <> show from <> " to " <> show to
  input <- testslices from (to + 1)
  parseSize evaluator input

parseHaydn :: _ => _ -> IO r
parseHaydn eval = do
  slices <- slicesFromFile haydn5
  parseSize eval $ slicesToPath $ take 9 slices

mainHaydn = do
  slices <- slicesFromFile haydn5
  derivs <- parseSize pvCount $ slicesToPath $ take 8 slices
  print derivs
  putStrLn "done."

mainRare = do
  slices <- slicesFromFile "data/theory-article/10c_rare_int.musicxml"
  putStrLn "\\documentclass[tikz]{standalone}"
  putStrLn "\\usetikzlibrary{calc,positioning}"
  putStrLn "\\tikzstyle{slice} = []"
  putStrLn "\\tikzstyle{transition} = []"
  putStrLn "\\begin{document}"
  putStrLn "\\begin{tikzpicture}[xscale=4,yscale=1]"
  derivs <- parse logTikz pvDerivUnrestricted $ slicesToPath slices
  putStrLn "\\end{tikzpicture}"
  putStrLn "\\end{document}"
  -- pure ()
  let ds = S.toList $ flattenDerivations derivs
  pics <- forM ds $ \d -> case replayDerivation derivationPlayerPVAllEdges d of
    Left err -> do
      putStrLn err
      print d
      return Nothing
    Right g -> return $ Just g
  viewGraphs "rare.tex" $ catMaybes pics

main = mainRare
