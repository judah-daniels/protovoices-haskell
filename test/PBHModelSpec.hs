{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PBHModelSpec where

import Musicology.Core
import Data.Ord
import Test.Hspec
import Common
import Control.Monad.Except (ExceptT,runExceptT, lift, throwError)
import Data.List
import Data.Maybe
  ( catMaybes,
    isNothing,
    fromMaybe,
    fromJust,
    mapMaybe,
    maybeToList,
  )
import Data.Vector qualified as V
import Internal.MultiSet qualified as MS
import Evaluator
import HeuristicParser
import HeuristicSearch
import PBHModel
import Language.Haskell.DoNotation
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled
import Heuristics
import PVGrammar hiding
  ( slicesFromFile,
  )
import PVGrammar.Generate
import PVGrammar.Parse
import Prelude hiding
  ( Monad (..),
    lift,
    pure,
  )
import Control.Monad.State (evalState)
import Control.Monad.Trans.Except (throwE)

type InputSlice ns = ([(ns, Music.RightTied)], Bool)

-- main :: IO ()
-- main = putStrLn "Test suite still not yet implemented"
--   params <- loadParams "preprocessing/dcml_params.json"
--   pure ()
  -- hspec mo

mostLikelyChordSpec :: Spec
mostLikelyChordSpec = do 
  params <- runIO $ loadParams "preprocessing/dcml_params.json"
  runIO $ do 
    let chordTypes = chordtypes params
    -- let chordTypes = chordtypes params
    
    -- showChordFromSlice params (genSlice' ["F3", "C3", "A4", "C5"])
    putStrLn $ "\nConsidering: " <> show sliceFmaj'
    let (r,f) = showChordFromSlice' params sliceFmaj'
    putStrLn $ "Transformed: " <> show (transposeSlice (spc $ r-14) sliceFmaj')
    putStrLn $ "Inferred: " <> f
            -- notes' = transposeSlice (spc root) notes

    -- putStrLn $ showChordFromSlice params (genSlice' ["D3", "D3", "F#4", "A5"])

    -- putStrLn $ "\nConsidering: " <> show (genSlice' ["D3", "F#4", "A5"])
    -- putStrLn $ showChordFromSlice params (genSlice' ["D3", "F#4", "A5"]) 

    let slc = (genSlice' ["D3", "F#4", "A5"])
    putStrLn $ "\nConsidering: " <> show slc
    let (r',f') = showChordFromSlice' params slc
    putStrLn $ "Transformed: " <> show (transposeSlice (spc (r'-14)) slc)
    putStrLn $ "Inferred: " <> f'
    -- putStrLn $ showChordFromSlice params sliceFmaj'
    -- putStrLn $ showChordFromSlice params (genSlice' ["B3", "D#3", "F#4", "B5"])

    -- putStrLn $ "Considering: " <> show (genSlice' ["B3", "D#3", "F#4", "B5"])
    -- putStrLn $ showChordFromSlice params (genSlice' ["B3", "D#3", "F#4", "B5"])
    let logLikelihoods = (reverse . sort $ sliceChordWeightedLogLikelihoods params slc)
    let prettyLogLikelihoods = map (\(p,r,t)-> show p <> ": " <> (showNotation $ spc (r-14)) <> chordTypes !! t) logLikelihoods

    putStrLn $ showChordFromSlice params (genSlice' ["Db3", "F3", "Ab4", "Db5"])
    mapM_ putStrLn (take 20 prettyLogLikelihoods)
    -- let lll = reverse . sort $ sliceChordLogLikelihoods params (sliceFmaj')
    -- let rrr = map (\(p,r,t)-> show p <> ": " <> (showNotation $ spc (r-14)) <> chordTypes !! t) lll
    -- print sliceFmaj'
    -- mapM_ (putStrLn) (take 6 rrr)
    --
    -- let lll = reverse . sort $ sliceChordWeightedLogLikelihoods params (sliceFmaj')
    -- let rrr = map (\(p,r,t)-> show p <> ": " <> (showNotation $ spc (r-14)) <> chordTypes !! t) lll
    -- print sliceFmaj'
    -- mapM_ (putStrLn) (take 6 rrr)

--     slc' = Notes $ Internal.MultiSet.map transformPitch (slc)
--       where
--         transformPitch ::
--           Music.SPitch -> SIC
--         transformPitch p = let q = spc (fifths p) in Music.pfrom q chordRootNote

  describe "Inferring Chord labels" $ do
    it "F major triad 1" $ 
      "FM" == showChordFromSlice params (genSlice' ["F3", "C3", "A4", "C5"])
    it "F major triad 2" $ 
      "FM" == showChordFromSlice params (genSlice' ["F3", "A3", "C5"])
    it "F major triad 3" $ 
      "FM" == showChordFromSlice params (genSlice' ["F3", "C3", "C9","A2",  "C5"])
    it "F major triad 4" $ 
      "FM" == showChordFromSlice params (genSlice' ["F4", "C5", "A3", "C5"])
    it "F major triad 5" $ 
      "FM" == showChordFromSlice params (genSlice' ["F4", "C3", "A4", "F5" ])
    it "B major triad 1" $ 
      "BM" == showChordFromSlice params (genSlice' ["B3", "D#3", "F#4", "B5"])
    it "B major triad 2" $ 
      "BM" == showChordFromSlice params (genSlice' ["B3", "D#3", "F#5"])
    it "F major triad 1" $ 
      "F♯M" == showChordFromSlice params (genSlice' ["F#3", "C#3", "A#4", "C#5"])
    it "F major triad 2" $ 
      "E♭M" == showChordFromSlice params (genSlice' ["Eb3", "G3", "Bb4"])
    it "Db major triad 1" $ 
      "D♭M" == showChordFromSlice params (genSlice' ["Db3", "F3", "Ab4", "Db5"])
  pure ()
 
showChordFromSlice' params slice = 
  let (root, chordType, prob) = mostLikelyChordFromSlice params slice in 
      (root, showNotation (spc (root - 14)) <> chordType)

showChordFromSlice params slice = 
  let (root, chordType, prob) = mostLikelyChordFromSlice params slice in 
      showNotation (spc (root - 14)) <> chordType

-- mostLikelyChordFromSlice' :: HarmonicProfileData -> Notes SPitch -> (SIC, String, Double)
-- mostLikelyChordFromSlice' hpData (Notes slc) = (sic root, chordtypes hpData !! chordTypeIndex, p)
--  where
--   (root, (chordTypeIndex, p)) = maximumBy (comparing (snd . snd)) (zip {- [0 ..] -} (mostLikelyChordType <$> sliceChordWeightedLogLikelihoods hpData notes))

mostLikelyChordType :: [Double] -> (Int, Double)
mostLikelyChordType chordTypeProbs = maximumBy (comparing snd) (zip [0 ..] chordTypeProbs)
---------------------------------- -------------------------------- -------------------------------- |
-- INPUTS FOR TESTING
--
-- The musical surface from Figure 4 as a sequence of slices and transitions.
-- Can be used as an input for parsing.
path321sus =
  Path [e nat 4, c nat 4] [(Inner $ c nat 4, Inner $ c nat 4)] $
    Path [d nat 4, c nat 4] [(Inner $ d nat 4, Inner $ d nat 4)] $
      Path [d nat 4, b nat 4] [] $
        PathEnd [c nat 4]

testInput :: [InputSlice SPC]
testInput =
  [ ([(e' nat, Music.Holds), (c' nat, Music.Ends)], False),
    ([(e' nat, Music.Holds)], True),
    ([(e' nat, Music.Ends)], False)
  ]

slices43 :: [InputSlice SPitch]
slices43 =
  [ ([(c nat 3, Music.Holds), (g nat 4, Music.Holds),(c nat 4, Music.Holds), (f nat 4, Music.Ends)], True),
    ([(c nat 3, Music.Ends), (g nat 4, Music.Ends),(c nat 4, Music.Ends), (e nat 4, Music.Ends)], False)
  ]

sliceFmaj :: Notes SPitch
sliceFmaj = fromJust $ genSlice ["F3", "C3", "A4", "C5"]

sliceFmaj' :: Notes SPitch
sliceFmaj' = fromJust $ genSlice ["F4", "C4", "A4"]

sliceFmaj4 :: Notes SPitch
sliceFmaj4 = fromJust $ genSlice ["F3", "C3", "Bb4", "C5"]

sliceD764 :: Notes SPitch
sliceD764 = fromJust $ genSlice ["A3", "D4", "C3", "F#4"]

genSlice' = fromJust . genSlice

genSlice :: [String] -> Maybe (Notes SPitch)
genSlice inputNotes = do 
  pitches <- mapM readNotation inputNotes
  pure $ Notes $ MS.fromList pitches


spec :: Spec
spec = do
  pure ()
