{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Parser
import           PVGrammar
import           PVGrammar.Parse
import           PVGrammar.Generate
import           Common
import           Display
import           Scoring

--import Musicology.Internal.Helpers
import           Musicology.MusicXML
import           Musicology.Core
import           Musicology.Core.Slicing
import           Musicology.Pitch.Spelled      as MT

import           Data.Ratio                     ( Ratio(..) )
import           Lens.Micro                     ( over )
import           Data.Maybe                     ( catMaybes )
import           Data.Either                    ( partitionEithers )

import qualified Data.MultiSet                 as MS
import qualified Data.Set                      as S
import qualified Data.Semiring                 as R
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Control.Monad                  ( forM
                                                , forM_
                                                )

-- utilities
-- =========

-- reading files
-- -------------

testfile =
  "/home/chfin/dateien/dev/haskell/work/haskell-musicology/musicology-musicxml/testdata/allemande.musicxml"

bb =
  "/home/chfin/dateien/dev/haskell/work/proto-voice-model/bluebossa.musicxml"

brahms1 =
  "/home/chfin/dateien/dev/haskell/work/proto-voice-model/brahms1.musicxml"

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
  -> Path (StartStop [Pitch i]) [Edge (ICOf i)]
slicesToPath slices = Path (:⋊) [] $ go $ normalizeTies slices
 where
  normalizeTies (s : next : rest) = (fixTie <$> s)
    : normalizeTies (next : rest)
   where
    nextNotes = fst <$> next
    fixTie (p, t) = if p `L.elem` nextNotes then (p, t) else (p, Ends)
  normalizeTies [s] = [map (fmap $ const Ends) s]
  normalizeTies []  = []
  mkSlice = Inner . fmap fst
  mkEdges notes = catMaybes $ mkEdge <$> notes
   where
    mkEdge (p, Ends ) = Nothing
    mkEdge (p, Holds) = let p' = pc p in Just (Inner p', Inner p')
  go (notes : rest) = Path (mkSlice notes) (mkEdges notes) $ go rest
  go []             = PathEnd (:⋉)

testslices from to =
  slicesToPath . drop (from - 1) . take to <$> slicesFromFile testfile

-- manual inputs
-- -------------

monopath :: [a] -> Path (StartStop [a]) [b]
monopath xs = Path (:⋊) [] $ go xs
 where
  go []            = PathEnd (:⋉)
  go (note : rest) = Path (Inner [note]) [] $ go rest

path :: [[a]] -> Path (StartStop [a]) [b]
path xs = Path (:⋊) [] $ go xs
 where
  go []             = PathEnd (:⋉)
  go (notes : rest) = Path (Inner notes) [] $ go rest

-- actions
-- -------

printDerivs path = do
  ds <- parseSilent pvDeriv path
  forM_ (flattenDerivations ds) $ \d -> do
    putStrLn "\nDerivation:"
    forM_ d $ \step -> do
      putStrLn $ "- " <> show step
    case replayDerivation d derivationPlayerPV of
      Left  error -> putStrLn $ "Error: " <> error
      Right _     -> putStrLn "Ok."

plotDerivs fn input = do
  derivs <- parseSilent pvDeriv input
  let ds = S.toList $ flattenDerivations derivs
  pics <- forM ds $ \d -> case replayDerivation d derivationPlayerPV of
    Left error -> do
      putStrLn error
      print d
      return Nothing
    Right g -> return $ Just g
  viewGraphs fn $ catMaybes pics

plotSteps fn deriv = do
  let graphs          = unfoldDerivation derivationPlayerPV deriv
      (errors, steps) = partitionEithers graphs
  mapM_ putStrLn errors
  viewGraphs fn $ reverse steps

-- example derivations
-- ===================

derivBrahms :: [PVLeftMost MT.SIC]
derivBrahms = buildDerivation $ do
  splitLeft $ mkSplit $ do
    splitT (:⋊) (:⋉) (c' shp) RootNote False False
    splitT (:⋊) (:⋉) (a' nat) RootNote False False
  hori $ mkHori $ do
    horiNote (a' nat) ToBoth     1
    horiNote (c' shp) (ToLeft 1) 0
    addPassing (c' shp) (a' nat)
  splitRight $ mkSplit $ do
    splitNT (c' shp) (a' nat) (b' nat) False False
    splitT (Inner $ a' nat) (Inner $ a' nat) (g' shp) FullNeighbor False False
  hori $ mkHori $ do
    horiNote (a' nat) (ToRight 1) 0
    horiNote (c' shp) (ToLeft 1)  0
    addPassing (c' shp) (a' nat)
  freeze FreezeOp
  splitLeft $ mkSplit $ do
    splitNT (c' shp) (a' nat) (b' nat) False False
  freeze FreezeOp
  freeze FreezeOp
  hori $ mkHori $ do
    horiNote (b' nat) (ToRight 1) 0
    horiNote (g' shp) (ToLeft 1)  0
  splitLeft $ mkSplit $ do
    addToRight (g' shp) (a' nat) SingleLeftNeighbor False
  freeze FreezeOp
  freeze FreezeOp
  splitLeft $ mkSplit $ do
    addToRight (b' nat) (c' shp) SingleLeftNeighbor False
  freeze FreezeOp
  freeze FreezeOp
  freeze FreezeOp
  freeze FreezeOp

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
  pics <- forM ds $ \d -> case replayDerivation d derivationPlayerPV of
    Left error -> do
      putStrLn error
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

mainResult evaluator from to = do
  putStrLn $ "slices " <> show from <> " to " <> show to
  input <- testslices from (to + 1)
  parseSize evaluator input

main = mainBB
