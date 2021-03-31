{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Parser
import           PVGrammar
import           Common
import           Display

--import Musicology.Internal.Helpers
import           Musicology.MusicXML
import           Musicology.Core
import           Musicology.Core.Slicing

import           Data.Ratio                     ( Ratio(..) )
import           Lens.Micro                     ( over )
import           Data.Maybe                     ( catMaybes )

import qualified Data.MultiSet                 as MS
import qualified Data.Set                      as S
import qualified Data.Semiring                 as R
import qualified Data.List                     as L

-- utilities
------------

testfile =
  "/home/chfin/dateien/dev/haskell/work/haskell-musicology/musicology-musicxml/testdata/allemande.musicxml"

bb =
  "/home/chfin/dateien/dev/haskell/work/proto-voice-model/bluebossa.musicxml"

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

-- mains
--------

mainTest from to = do
  putStrLn $ "slices " <> show from <> " to " <> show to
  input <- testslices from (to + 1)
  print input
  count <- parse' pvCount input
  putStrLn $ show count <> " derivations"

mainBB = do
  input <- slicesToPath <$> slicesFromFile bb
  print input
  count <- parse' pvCount input
  print count

mainGraph = do
  input <- testslices 0 10
  print input
  derivs <- parse' pvDeriv input
  let d = S.findMin $ flattenDerivations derivs
  mapM_ print d
  case replayDerivation d derivationPlayerPV of
    Left error -> do
      putStrLn error
      -- print derivs
    Right g -> putStrLn $ "\n" <> tikzDerivationGraph showTex showTex g

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
  parse' evaluator input

main = mainGraph
