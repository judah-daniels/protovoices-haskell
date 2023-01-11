module Main where

import Common
import Data.Maybe (catMaybes, mapMaybe)
import HeuristicParser
import HeuristicSearch
import Musicology.Core
import Musicology.Pitch.Spelled
import PBHModel
import PVGrammar
import PVGrammar.Parse
import Test.Hspec

pathFromSlices
  :: Eval (Edges SPC) [Edge SPC] (Notes SPC) [SPC] (PVLeftmost SPC)
  -> [([(SPC, Bool)], Bool)]
  -> Path (Maybe [Edge SPC], Bool) (Slice (Notes SPC))
pathFromSlices eval = reversePath . mkPath Nothing
 where
  mkPath
    :: Maybe [Edge SPC]
    -> [([(SPC, Bool)], Bool)]
    -> Path (Maybe [Edge SPC], Bool) (Slice (Notes SPC))

  mkPath eLeft ((slice, boundary) : rst) = Path (eLeft, boundary) (Slice $ evalSlice' (map fst slice)) $ mkPath (Just $ getTiedEdges slice) rst
  mkPath eLeft [] = PathEnd (Nothing, False)

  evalSlice' :: [SPC] -> Notes SPC
  evalSlice' = evalSlice eval

  getTiedEdges :: [(SPC, Bool)] -> [Edge SPC]
  getTiedEdges = mapMaybe mkTiedEdge
   where
    mkTiedEdge :: (SPC, Bool) -> Maybe (Edge SPC)
    mkTiedEdge (_, False) = Nothing
    mkTiedEdge (pitch, True) = Just (Inner pitch, Inner pitch)

-- | The musical surface of a 321 sus progression as a sequence of slices
slices321sus :: [([(SPC, Bool)], Bool)]
slices321sus =
  [ ([(e' nat, False), (c' nat, True)], True)
  , ([(d' nat, True), (c' nat, False)], False)
  , ([(d' nat, False), (b' nat, False)], False)
  , ([(c' nat, False)], False)
  ]

-- | The musical surface of a 321 sus progression as a sequence of slices and transitions, ready for parsing
path321sus :: Path (Maybe [Edge SPC], Bool) (Slice (Notes SPC))
path321sus =
  Path (Nothing, False) (Slice $ evalSlice' [c' nat]) $
    Path (Just [], False) (Slice $ evalSlice' [d' nat, b' nat]) $
      Path (Just [(Inner $ d' nat, Inner $ d' nat)], False) (Slice $ evalSlice' [d' nat, c' nat]) $
        Path (Just [(Inner $ c' nat, Inner $ c' nat)], False) (Slice $ evalSlice' [e' nat, c' nat]) $
          PathEnd (Nothing, True)
 where
  evalSlice' = evalSlice eval

  eval :: Eval (Edges SPC) [Edge SPC] (Notes SPC) [SPC] (PVLeftmost SPC)
  eval = protoVoiceEvaluator

main :: IO ()
main = do
  -- finalPath <- runHeuristicSearch protoVoiceEvaluator slices321sus chords321sus
  (finalPath, ops) <- runHeuristicSearch params protoVoiceEvaluator (testHeuristic params) slices chords

  let res = evalPath finalPath chords params
  -- let res = evalPath finalPath chords321sus hpData

  putStrLn $ "\nFinal Path: " <> show finalPath
  putStrLn $ "\nEvaluation score: " <> show res
  hspec pathFromSlicesSpec

pathFromSlicesSpec :: Spec
pathFromSlicesSpec = describe "Test setting up path from slices" $ do
  it "Should match the manually created path" $
    pathFromSlices protoVoiceEvaluator slices321sus `shouldBe` path321sus

spec :: Spec
spec = do
  pure ()
