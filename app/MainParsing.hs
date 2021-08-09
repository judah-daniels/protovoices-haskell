{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-all #-}
module Main where

import           Parser
import           PVGrammar
import           PVGrammar.Parse
import           PVGrammar.Generate
import           Common
import           Display
import           ScoresCommon

--import Musicology.Internal.Helpers
import           Musicology.MusicXML
import           Musicology.Core
import           Musicology.Core.Slicing
import           Musicology.Pitch.Spelled      as MT

import           Data.Ratio                     ( Ratio(..) )
import           Lens.Micro                     ( over )
import           Data.Maybe                     ( catMaybes )
import           Data.Either                    ( partitionEithers )

import qualified Internal.MultiSet             as MS
import qualified Data.HashMap.Strict           as HS
import qualified Data.Set                      as S
import qualified Data.Semiring                 as R
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Control.Monad                  ( forM
                                                , forM_
                                                )


-- better do syntax
import qualified Language.Haskell.DoNotation   as Do
-- import           Prelude                 hiding ( Monad(..)
--                                                 , pure
--                                                 )
import           Prelude
import           Data.String                    ( fromString )
import           Control.DeepSeq                ( force
                                                , deepseq
                                                )

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

getPitchGroups :: FilePath -> IO [[OnOff SPitch (Ratio Int)]]
getPitchGroups file = do
  txt <- readFile file
  return
    $   fmap (fmap $ over onOffContent pitch)
    $   onOffGroups
    $   asNote
    <$> xmlNotesHeard txt

slicesFromFile :: FilePath -> IO [[(SPitch, RightTied)]]
slicesFromFile file = do
  txt <- readFile file
  let notes  = asNote <$> xmlNotesHeard txt
      slices = slicePiece tiedSlicer notes
  return $ mkSlice <$> filter (not . null) slices
 where
  mkSlice notes = mkNote <$> notes
  mkNote (note, tie) = (pitch note, rightTie tie)

slicesToPath
  :: (Interval i, Ord (ICOf i), Eq i)
  => [[(Pitch i, RightTied)]]
  -> Path [Pitch (ICOf i)] [Edge (Pitch (ICOf i))]
slicesToPath = go
 where
  -- normalizeTies (s : next : rest) = (fixTie <$> s)
  --   : normalizeTies (next : rest)
  --  where
  --   nextNotes = fst <$> next
  --   fixTie (p, t) = if p `L.elem` nextNotes then (p, t) else (p, Ends)
  -- normalizeTies [s] = [map (fmap $ const Ends) s]
  -- normalizeTies []  = []
  mkSlice = fmap (pc . fst)
  mkEdges notes = catMaybes $ mkEdge <$> notes
   where
    mkEdge (p, Ends ) = Nothing
    mkEdge (p, Holds) = let p' = pc p in Just (Inner p', Inner p')
  go []             = error "cannot construct path from empty list"
  go [notes       ] = PathEnd (mkSlice notes)
  go (notes : rest) = Path (mkSlice notes) (mkEdges notes) $ go rest

testslices from to =
  slicesToPath . drop (from - 1) . take to <$> slicesFromFile testfile

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

-- example derivations
-- ===================

derivBrahms :: [PVLeftMost (Pitch MT.SIC)]
derivBrahms = buildDerivation $ do
  split $ mkSplit $ do
    splitT (:⋊) (:⋉) (c' shp) RootNote False False
    splitT (:⋊) (:⋉) (a' nat) RootNote False False
  hori $ mkHori $ do
    horiNote (a' nat) ToBoth     True
    horiNote (c' shp) (ToLeft 1) False
    addPassing (c' shp) (a' nat)
  splitRight $ mkSplit $ do
    splitNT (c' shp) (a' nat) (b' nat) PassingMid False False
    splitT (Inner $ a' nat) (Inner $ a' nat) (g' shp) FullNeighbor False False
  hori $ mkHori $ do
    horiNote (a' nat) (ToRight 1) False
    horiNote (c' shp) (ToLeft 1)  False
    addPassing (c' shp) (a' nat)
  freeze FreezeOp
  split $ mkSplit $ do
    splitNT (c' shp) (a' nat) (b' nat) PassingMid False False
  freeze FreezeOp
  freeze FreezeOp
  hori $ mkHori $ do
    horiNote (b' nat) (ToRight 1) False
    horiNote (g' shp) (ToLeft 1)  False
  split $ mkSplit $ do
    addToRight (g' shp) (a' nat) SingleLeftNeighbor False
  freeze FreezeOp
  freeze FreezeOp
  split $ mkSplit $ do
    addToRight (b' nat) (c' shp) SingleLeftNeighbor False
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
  hori $ HoriScore $ Do "h2"
  split $ SplitScore $ Do "s"
  freeze $ FreezeScore $ Do "L1"
  hori $ HoriScore $ Do "h1"
  freeze $ FreezeScore $ Do "L2"
  freeze $ FreezeScore $ Do "M"
  freeze $ FreezeScore $ Do "C"
  freeze $ FreezeScore $ Do "R"
  where (>>) = (Do.>>)


-- mains
-- =====

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
  putStrLn "\nslices:"
  mapM_ print $ vcGetByLength vc (n - 1)

mainResult
  :: Parsable e a v
  => Eval e [Edge (Pitch SIC)] a [Pitch SIC] v
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

main = mainHaydn
