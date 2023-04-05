module Main where

import Control.Exception (evaluate)
import EvaluationSpec
import FileHandlingSpec
import HeuristicSearchSpec
import HeuristicSpec
import PBHModelSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  heuristicSpec

-- mostLikelyChordSpec

-- fullParseSpec
-- fileHandlingSpec

-- evaluationSpec

-- pbhModelSpec

--
