module Main where

import Control.Exception (evaluate)

-- import EvaluationSpec
-- import FileHandlingSpec
-- import HeuristicSearchSpec
-- import HeuristicSpec

import HarmonySpec
import PBHModelSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  -- pbhModelSpec

  -- heuristicSpec
  harmonySpec

  mostLikelyChordSpec

-- fullParseSpec
-- fileHandlingSpec

-- evaluationSpec

--
