module Main where

import Control.Exception (evaluate)
import EvaluationSpec
import HeuristicSearchSpec
import HeuristicSpec
import PBHModelSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  -- mostLikelyChordSpec

  fullParseSpec

-- evaluationSpec

-- heuristicSpec

-- pbhModelSpec

--