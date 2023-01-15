{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PBHModelSpec where


import Musicology.Core
import Test.Hspec
import Common
import Control.Monad.Except (ExceptT,runExceptT, lift, throwError)
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
  runIO $ do 
    params <- loadParams "preprocessing/dcml_params.json"
    print $ mostLikelyChordFromSlice params sliceFmaj
    print $ mostLikelyChordFromSlice params sliceFmaj4
    print $ mostLikelyChordFromSlice params sliceD764
  pure ()

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

sliceFmaj4 :: Notes SPitch
sliceFmaj4 = fromJust $ genSlice ["F3", "C3", "Bb4", "C5"]

sliceD764 :: Notes SPitch
sliceD764 = fromJust $ genSlice ["A3", "D4", "C3", "F#4"]

genSlice :: [String] -> Maybe (Notes SPitch)
genSlice inputNotes = do 
  pitches <- mapM readNotation inputNotes
  pure $ Notes $ MS.fromList pitches


spec :: Spec
spec = do
  pure ()
