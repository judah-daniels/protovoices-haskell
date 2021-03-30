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

main = print "Hi!"

-- utilities
------------

testfile =
  "/home/chfin/dateien/dev/haskell/work/haskell-musicology/musicology-musicxml/testdata/allemande.musicxml"

getPitchGroups :: FilePath -> IO [[OnOff SPitch (Ratio Int)]]
getPitchGroups file = do
  txt <- readFile file
  return
    $   fmap (fmap $ over onOffContent pitch)
    $   onOffGroups
    $   asNote
    <$> xmlNotesHeard txt

slicesFromFile' :: FilePath -> IO [[(SPitch, RightTied)]]
slicesFromFile' file = do
  txt <- readFile file
  let notes  = asNote <$> xmlNotesHeard txt
      slices = slicePiece tiedSlicer notes
  return $ mkSlice <$> slices
 where
  mkSlice notes = mkNote <$> notes
  mkNote (note, tie) = (pitch note, rightTie tie)

slicesToPath
  :: (Interval i, Ord (ICOf i))
  => [[(Pitch i, RightTied)]]
  -> Path (StartStop (Notes (ICOf i))) [Edge (ICOf i)]
slicesToPath slices = Path (:⋊) [] $ go slices
 where
  mkSlice = Inner . Notes . MS.fromList . fmap (pc . fst)
  mkEdges notes = catMaybes $ mkEdge <$> notes
   where
    mkEdge (p, Ends ) = Nothing
    mkEdge (p, Holds) = let p' = pc p in Just (Inner p', Inner p')
  go (notes : rest) = Path (mkSlice notes) (mkEdges notes) $ go rest
  go []             = PathEnd (:⋉)

testslices from to =
  slicesToPath . drop (from - 1) . take to <$> slicesFromFile' testfile

