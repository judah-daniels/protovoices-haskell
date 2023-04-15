{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HeuristicSpec where

import System.TimeIt qualified as Time
import System.Timeout
import FileHandling
import Core
import HeuristicParser
import PBHModel
import Musicology.Core
import Test.Hspec
import Control.Logging qualified as Log

heuristicSpec :: Spec
heuristicSpec = runIO $ Log.withStdoutLogging $ do
  params <- loadParams "preprocessing/dcml_params.json"
  let corpus = "tests"
      pieceName = "shortest"
      inputPath = "preprocessing/inputs/"
      outputPath = "preprocessing/outputs/"
      a = Heuristic1

  let outputFile = "test" <> corpus <> "/" <> pieceName <> ".json"
  inputChords <- chordsFromFile (inputPath <> "chords/" <> corpus <> "/" <> pieceName <> ".csv")
  inputSlices <- slicesFromFile' (inputPath <> "slices/" <> corpus <> "/" <> pieceName <> ".csv")

  let scorer = scoreSegments params scoreSegment'

  m <- timeout timeOutMs $ Time.timeItT $ runAlgo a scorer params inputChords inputSlices

  res <- case m of 
          Nothing -> pure $ nullResultToJSON a
          Just (time, r) -> resultToJSON a time r

  writeJSONToFile outputFile $ concatResults corpus pieceName inputChords [res]

