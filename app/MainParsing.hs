{-# LANGUAGE FlexibleContexts #-}
module Main where

--import Musicology.Internal.Helpers
import           Musicology.MusicXML
import           ParserOld
import           PVGrammar
import           ParserViz
import           Musicology.Core
import           Musicology.Core.Slicing

import qualified Data.Set                      as S
import           Data.Ratio                     ( Ratio )
import           Diagrams.Prelude        hiding ( (^-^)
                                                , (^+^)
                                                )

main = testslices 1 13 >>= print . countDerivations

-- test = do
--   derivs <- countDerivations <$> testslices 1 9
--   if derivs == 304
--     then putStrLn "success"
--     else putStrLn $ "failure, counted " <> show derivs <> " derivations"

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

getGroups :: FilePath -> IO [[OnOff (Note SInterval (Ratio Int)) (Ratio Int)]]
getGroups file = do
  txt <- readFile file
  return $ onOffGroups $ asNote <$> xmlNotesHeard txt

slicesFromFile file = do
  groups <- getPitchGroups file
  let slices = groupsToSlices cleanSlicer groups
  return $ mkNotes . S.fromList . fmap pc <$> slices

slicesFromFile' :: FilePath -> IO [[(SPitch, RightTied)]]
slicesFromFile' file = do
  txt <- readFile file
  let notes  = asNote <$> xmlNotesHeard txt
      slices = slicePiece tiedSlicer notes
  return $ mkSlice <$> slices
 where
  mkSlice notes = mkNote <$> notes
  mkNote (note, tie) = (pitch note, rightTie tie)

testGroups = getPitchGroups testfile

testslices from to = drop (from - 1) . take to <$> slicesFromFile testfile

-- runTest = do
--   s <- slicesFromFile testfile
--   return $ isMusic (take 4 s)

showDerivations :: IO [Notes SIC] -> IO ()
showDerivations slices = do
  s <- slices
  mapM_ (putStrLn . showFoldable) $ take 2 $ S.toList $ allDerivations s

renderFirstGraph slices = do
  s <- slices
  let graph = head $ S.toList $ allGraphs s
  myRender $ graphDiag graph

renderGraphs slices = do
  s <- slices
  let graphs = take 20 $ S.toList $ allGraphs s
  myRender $ foldl (===) mempty (graphDiag <$> graphs)

renderSumGraph slices = do
  s <- slices
  let (sg, counts) = sumGraphs $ allGraphs s
  myRender $ sumGraphDiag sg counts

showSum slices = do
  s <- slices
  let (sg, counts) = sumGraphs $ allGraphs s
  putStrLn $ showFoldable counts

showFirstGraph slices = do
  s <- slices
  let graph = head $ S.toList $ allGraphs s
  print graph

showGraphs :: IO [Notes SIC] -> IO ()
showGraphs slices = do
  s <- slices
  mapM_ print $ take 2 $ S.toList $ allGraphs s

-- Horizontalize: [2,(D,B,F♯)@N,6] -> [2,(D,B)@T,2] [2,(D,B)@T,2-(D-D,B-B)-3,(D,B,F♯,C♯)@N,6] [3,(D,B,F♯)@N,6]
