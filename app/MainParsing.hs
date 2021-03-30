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
  :: (Interval i, Ord (ICOf i))
  => [[(Pitch i, RightTied)]]
  -> Path (StartStop [Pitch i]) [Edge (ICOf i)]
slicesToPath slices = Path (:⋊) [] $ go slices
 where
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

mainTest = do
  let from = 9
      to   = 21
  putStrLn $ "slices " <> show from <> " to " <> show to
  input <- testslices from (to + 1)
  print input
  count <- parse pvCount input
  putStrLn $ show count <> " derivations"

mainBB = do
  input <- slicesToPath <$> slicesFromFile bb
  print input
  count <- parse pvCount input
  print count

mainGraph = do
  derivs <- testslices 0 4 >>= parse pvDeriv
  let d        = S.findMin $ flattenDerivations derivs
      (Just g) = replayDerivation d derivationPlayerUnit
  putStrLn ""
  putStrLn $ tikzDerivationGraph (const "s") (const "e") g

main = mainTest
