module Main where

import Control.Exception (evaluate)
import HeuristicSearchSpec
import HeuristicSpec
import PBHModelSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  -- mostLikelyChordSpec
  -- heuristicSpec
  fullParseSpec

--
