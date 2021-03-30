{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Parser
import           PVGrammar
import           Common                         ( StartStop(..) )

--import Musicology.Internal.Helpers
import           Musicology.MusicXML
import           Musicology.Core
import           Musicology.Core.Slicing

import           Data.Ratio                     ( Ratio(..) )
import qualified Data.List.NonEmpty            as NL
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Lens.Micro                     ( over )
import qualified Data.MultiSet                 as MS
import           Data.Maybe                     ( catMaybes )

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
  let from = 0
      to   = 21
  putStrLn $ "slices " <> show from <> " to " <> show to
  input <- testslices from (to + 1)
  print input
  count <- parse pvCount input
  putStrLn $ show count <> " derivations"

mainBB = do
  input <- slicesFromFile bb
  count <- parse pvCount (slicesToPath input)
  print count

main = mainTest
