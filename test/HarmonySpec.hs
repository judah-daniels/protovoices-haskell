{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HarmonySpec where

import Common
import Harmony
import Harmony.ChordLabel
import Harmony.Params

import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Control.Monad.State (evalState)
import Control.Monad.Trans.Except (throwE)
import Data.List
import Data.Maybe
  ( catMaybes,
    fromJust,
    fromMaybe,
    isNothing,
    mapMaybe,
    maybeToList,
  )
import Data.Ord
import Data.Vector qualified as V
import HeuristicParser
import Algorithm.HeuristicSearch
import Heuristics
import Internal.MultiSet qualified as MS
import Language.Haskell.DoNotation
import Musicology.Core
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled
import PVGrammar hiding
  ( slicesFromFile,
  )
import PVGrammar.Generate
import PVGrammar.Parse
import Test.Hspec
import Prelude hiding
  ( Monad (..),
    lift,
    pure,
  )
import Debug.Trace (trace)
import PBHModel (mostLikelyChordFromSlice)

type InputSlice ns = ([(ns, Music.RightTied)], Bool)

-- main :: IO ()
-- main = putStrLn "Test suite still not yet implemented"
--   pure ()
-- hspec mo

-- probDensitySpec :: Spec
-- probDensitySpec = do
--   describe "multinomialLogProb" $ do
--
--     it "returns the correct log-probability for a basic input" $ do
--       let xs = V.fromList [2, 3, 1]
--           probs = V.fromList [0.3, 0.5, 0.2]
--           result = multinomialLogProb xs probs
--           expected = -2.0024805005437063
--       result `shouldBe` expected
--
--     it "returns -100000000 for an empty input" $ do
--       let xs = V.fromList []
--           probs = V.fromList [0.3, 0.5, 0.2]
--           result = multinomialLogProb xs probs
--           expected = -100000000
--       result `shouldBe` expected
--
--     -- it "returns an error message for vectors of unequal length" $ do
--     --   let xs = V.fromList [2, 3, 1]
--     --       probs = V.fromList [0.3, 0.5]
--     --       result = trace (show result) (multinomialLogProb xs probs)
--     --       expected = "Vectors must be of the same length."
--     --   result `shouldBe` expected
--
--     it "returns the correct log-probability for a large input" $ do
--       let xs = V.fromList [100000, 50000, 25000]
--           probs = V.fromList [0.3, 0.5, 0.2]
--           result = multinomialLogProb xs probs
--           expected = -28055.13760597352 
--       result `shouldBe` expected
--
harmonySpec :: Spec
harmonySpec = do
  -- probDensitySpec
  runIO $ do
    -- let chordTypes = chordtypes

    putStrLn $ "\nConsidering: " <> show sliceFmaj'
    putStrLn $ showChordFromSlice sliceFmaj'
    let d = fromJust $ genSlice ["D3", "F#3", "A#4"]
    putStrLn $ "\nConsidering: " <> show d
    putStrLn $ showChordFromSlice d
    print $ (genSlice' ["D3", "F#3", "A4", "C4"])
    print $ take 5 $ reverse . sort . V.toList $ allLabelLikelihoodsGivenSlice (genSlice' ["D3", "F#3", "A4", "C4"])
    -- print $ PBH.sliceChordWeightedLogLikelihoods params (genSlice' ["D3", "F#3", "A4", "C4"])
    -- pallLabelLikelihoodsGivenSlice
    -- let (r, f) = showChordFromSlice' params sliceFmaj'
    -- putStrLn $ "Transformed: " <> show (transposeSlice (spc $ r - 14) sliceFmaj')
    -- putStrLn $ "Inferred: " <> f
    -- notes' = transposeSlice (spc root) notes
    -- putStrLn $ showChordFromSlice params (genSlice' ["D3", "D3", "F#4", "A5"])

    -- putStrLn $ "\nConsidering: " <> show (genSlice' ["D3", "F#4", "A5"])
    -- putStrLn $ ghowChordFromSlice params (genSlice' ["D3", "F#4", "A5"])

    -- let slc = genSlice' ["C#3", "F3", "B4", "G#7"]
    -- putStrLn $ "\nConsidering: " <> show slc
    -- let (r', f') = showChordFromSlice' params slc
    -- putStrLn $ "Transformed: " <> show (transposeSlice (spc (r' - 14)) slc)
    -- putStrLn $ "Inferred: " <> f'

    -- putStrLn $ showChordFromSlice params sliceFmaj'
    -- putStrLn $ showChordFromSlice params (genSlice' ["B3", "D#3", "F#4", "B5"])

    -- putStrLn $ "Considering: " <> show (genSlice' ["B3", "D#3", "F#4", "B5"])
    -- putStrLn $ showChordFromSlice params (genSlice' ["B3", "D#3", "F#4", "B5"])
    -- let logLikelihoods = (reverse . sort $ sliceChordWeightedLogLikelihoods params slc)
    -- let prettyLogLikelihoods = map (\(p, r, t) -> show p <> ": " <> (showNotation $ spc (r - 14)) <> chordTypes !! t) logLikelihoods

    -- putStrLn $ showChordFromSlice params (genSlice' ["Db3", "F3", "Ab4", "Db5"])
    -- mapM_ putStrLn (take 20 prettyLogLikelihoods)
  -- let lll = reverse . sort $ sliceChordLogLikelihoods params (sliceFmaj')
  -- let rrr = map (\(p,r,t)-> show p <> ": " <> (showNotation $ spc (r-14)) <> chordTypes !! t) lll
  -- print sliceFmaj'
  -- mapM_ (putStrLn) (take 6 rrr)
  --
  -- let lll = reverse . sort $ sliceChordWeightedLogLikelihoods params (sliceFmaj')
  -- let rrr = map (\(p,r,t)-> show p <> ": " <> (showNotation $ spc (r-14)) <> chordTypes !! t) lll
  -- print sliceFmaj'
  -- mapM_ (putStrLn) (take 6 rrr)

      -- slc' = Notes $ Internal.MultiSet.map transformPitch (slc)
      --   where
      --     transformPitch ::
      --       Music.SPitch -> SIC
      --     transformPitch p = let q = spc (fifths p) in Music.pfrom q chordRootNote

  describe "Inferring Major Chord labels" $ do
    it "F major triad 1" $
      "FM" == showChordFromSlice (genSlice' ["F3", "C3", "A4", "C5"])
    it "F major triad 2" $
      "FM" == showChordFromSlice (genSlice' ["F3", "A3", "C5"])
    it "F major triad 3" $
      "FM" == showChordFromSlice (genSlice' ["F3", "C3", "C9", "A2", "C5"])
    it "F major triad 4" $
      "FM" == showChordFromSlice (genSlice' ["F4", "C5", "A3", "C5"])
    it "F major triad 5" $
      "FM" == showChordFromSlice (genSlice' ["F4", "C3", "A4", "F5"])
    it "B major triad 1" $
      "BM" == showChordFromSlice (genSlice' ["B3", "D#3", "F#4", "B5"])
    it "B major triad 2" $
      "BM" == showChordFromSlice (genSlice' ["B3", "D#3", "F#5"])
    it "F♯ major triad" $
      "F♯M" == showChordFromSlice (genSlice' ["F#3", "C#3", "A#4", "C#5"])
    it "E♭ major triad" $
      "E♭M" == showChordFromSlice (genSlice' ["Eb3", "G3", "Bb4"])
    it "Db major triad 1" $
      "D♭M" == showChordFromSlice (genSlice' ["Db3", "F3", "Ab4", "Db5"])

  describe "Infering Dominant Chord labels" $ do
    it "D64" $
      "DMm7" == showChordFromSlice (genSlice' ["D3", "F#3", "A4", "C4"])
    it "F#64" $
      "F♯Mm7" == showChordFromSlice (genSlice' ["A#3", "F#3", "E4", "C#4"])
    it "E64" $
      "EMm7" == showChordFromSlice (genSlice' ["E3", "G#3", "D4", "B4"])
    it "C#Mm7" $ do
      "C♯Mm7" == showChordFromSlice (genSlice' ["C#3", "F3", "B4", "G#7"])

  describe "Infering Minor Chord labels" $ do
    it "Dm" $
      "Dm" == showChordFromSlice  (genSlice' ["D3", "F3", "A4"])
    it "Fm" $
      "Fm" == showChordFromSlice  (genSlice' ["Ab3", "F3", "C4", "C4"])
    it "Em"$
      "Em" == showChordFromSlice  (genSlice' ["E3", "G3", "E2", "B4"])
    it "Cm" $ do
      "Cm" == showChordFromSlice  (genSlice' ["C3", "G3", "Eb4", "Eb7"])

  describe "Generating Note Vector" $ do
    it "C" $
      V.replicate 29 0 V.// [(14,1::Double)] 
        == fromJust (noteVector (spelledp 0 3))
    it "Bb" $
      V.replicate 29 0 V.// [(12,1::Double)] 
        == fromJust (noteVector (spelledp (-2) 0))
    it "F#" $
      V.replicate 29 0 V.// [(14+6,1::Double)] 
        == fromJust (noteVector (spelledp 6 3))
    it "Out of bounds -15" $
        isNothing (noteVector (spelledp (-15) 3))
    it "Out of bounds 15" $
        isNothing (noteVector (spelledp 15 3))

  describe "Generating SliceVector" $ do
    it "CCG" $
      V.replicate 29 0 V.// ([(14,2), (15,1)] :: [(Int,Double)])
        == notesVector (Notes $ MS.fromList [spelledp 0 3, spelledp 0 7, spelledp 1 2])
    it "F A Bb E" $
      V.replicate 29 0 V.// ([(13,1), (17,1), (12, 1), (18, 1)] :: [(Int,Double)])
        == notesVector (Notes $ MS.fromList [spelledp (-2) 3, spelledp (-1) 7, spelledp (3) 2, spelledp 4 8])

  describe "Parent Likelihoods" $ do
    it "CCG" $
      V.replicate 29 0 V.// ([(14,2), (15,1)] :: [(Int,Double)])
        == notesVector (Notes $ MS.fromList [spelledp 0 3, spelledp 0 7, spelledp 1 2])
    it "F A Bb E" $
      V.replicate 29 0 V.// ([(13,1), (17,1), (12, 1), (18, 1)] :: [(Int,Double)])
        == notesVector (Notes $ MS.fromList [spelledp (-2) 3, spelledp (-1) 7, spelledp (3) 2, spelledp 4 8])
  -- Note this should definitely be a mixture model
  -- describe "Infering Aug Chord labels" $ do
  --   it "D+" $
  --     showChordFromSlice params (genSlice' ["D3", "F#3", "A#4"]) `elem` ["D+","F♯+", "A♯+"]
  --   it "C+" $
  --     showChordFromSlice params (genSlice' ["C3", "E3", "G#4"]) `elem` ["C+","G♯+", "E♯+"]

-- showChordFromSlice' pparamsarams slice =
--   let (root, chordType, prob) = mostLikelyChordFromSlice params slice
--    in (root, showNotation root <> chordType)

showChordFromSlice slice = trace ( show . map snd . take 5 . reverse . sort . V.toList $ allLabelLikelihoodsGivenSlice slice )
   show $ mostLikelyLabelFromSlice slice
   -- in shownotation root <> chordtype

-- showChordfromslice params slice =
--   let (root, chordtype, prob) = mostlikelychordfromslice params slice
--    in shownotation root <> chordtype

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
  [ ([(c nat 3, Music.Holds), (g nat 4, Music.Holds), (c nat 4, Music.Holds), (f nat 4, Music.Ends)], True),
    ([(c nat 3, Music.Ends), (g nat 4, Music.Ends), (c nat 4, Music.Ends), (e nat 4, Music.Ends)], False)
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
