{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Common                         ( Analysis(..)
                                                , Path(..)
                                                )
import           Control.Monad                  ( foldM
                                                , forM
                                                , forM_
                                                , replicateM
                                                , zipWithM_
                                                )
import           Control.Monad.Except           ( runExceptT )
import           Data.Either                    ( rights )
import           Data.List                      ( unzip4 )
import           Data.Maybe                     ( catMaybes
                                                , listToMaybe
                                                )
import qualified Data.Vector                   as V
import           GHC.Float                      ( int2Double )
import qualified GreedyParser                  as Greedy
import           Inference.Conjugate            ( Hyper
                                                , Trace
                                                , evalTracePredLogP
                                                , getPosterior
                                                , traceTrace
                                                , uniformPrior
                                                )
import           Musicology.Pitch               ( SPitch )
import           PVGrammar                      ( Edge
                                                , PVAnalysis
                                                , loadAnalysis
                                                , loadInput
                                                , topEdges
                                                )
import           PVGrammar.Parse                ( protoVoiceEvaluator )
import           PVGrammar.Prob.Simple          ( PVParams
                                                , observeDerivation'
                                                , sampleDerivation'
                                                )
import qualified Statistics.Sample             as Stats
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import qualified System.FilePattern            as FP
import qualified System.FilePattern.Directory  as FP
import           System.Random.Stateful         ( StatefulGen
                                                , initStdGen
                                                , newIOGenM
                                                )
import           Text.Pretty.Simple             ( CheckColorTty(..)
                                                , OutputOptions(..)
                                                , defaultOutputOptionsNoColor
                                                , pPrintOpt
                                                )

prettyPrint :: (Show a) => a -> IO ()
prettyPrint = pPrintOpt
  NoCheckColorTty
  (defaultOutputOptionsNoColor { outputOptionsCompact = True })

dataDir :: FilePath
dataDir = "data/"

main :: IO ()
main = do
  -- initialize
  genPure <- initStdGen
  gen     <- newIOGenM genPure
  let prior = uniformPrior @PVParams
  -- load data
  articleExamples <- loadDir $ dataDir </> "theory-article"
  Just bwv939     <- loadItem (dataDir </> "bach" </> "fünf-kleine-präludien")
                              "BWV_0939"
  Just bwv940 <- loadItem (dataDir </> "bach" </> "fünf-kleine-präludien")
                          "BWV_0940"
  let dataset = bwv939 : bwv940 : articleExamples
  forM_ dataset $ \(name, _ana, _trace, _surface) -> do
    putStrLn name
  -- cross validation
  let splits = leaveOneOut dataset
  crossPerps <- forM splits (comparePerNote gen prior)
  let (logpPriors, logpPosts, counts, basemeans) = unzip4 crossPerps
      count          = sum counts
      logppnPrior    = sum logpPriors / count
      logppnTrained  = sum logpPosts / count
      logppnBaseline = sum basemeans / count
      -- normalized per split (this gives too much weight to small test pieces)
      logppns        = fmap (\(_, logp, count, _) -> logp / count) crossPerps
      nsplits        = int2Double $ length splits
      meanlogppn     = sum logppns / nsplits
  putStrLn $ "prior logppn: " <> show logppnPrior
  putStrLn $ "prior perppn: " <> show (exp $ negate logppnPrior)
  putStrLn $ "overall trained logppn: " <> show logppnTrained
  putStrLn $ "overall trained perppn: " <> show (exp $ negate logppnTrained)
  putStrLn $ "baseline logppn: " <> show logppnBaseline
  putStrLn $ "baseline perppn: " <> show (exp $ negate logppnBaseline)
  -- these numbers don't really make sense because they give too much weight to small test pieces:
  putStrLn $ "mean trained logppn: " <> show meanlogppn
  putStrLn $ "mean trained perppn: " <> show (exp $ negate meanlogppn)

mainNaive dataset = do
  let prior = uniformPrior @PVParams
  posterior <- learn prior dataset
  -- prettyPrint posterior
  -- compare probabilities
  putStrLn "name\t\tprior\t\t\tposterior\t\tdifference"
  let diffs = predDiff prior posterior <$> dataset
      total = sum diffs
  putStrLn $ "total Δlogp: " <> show total
  putStrLn $ "total Δp: " <> show (exp total)


-- loading data
-- ------------

type Piece
  = (String, PVAnalysis SPitch, Trace PVParams, Path [SPitch] [Edge SPitch])

loadItem :: FilePath -> FilePath -> IO (Maybe Piece)
loadItem dir name = do
  ana <- loadAnalysis (dir </> name <.> "analysis.json")
  case ana of
    Left  _err -> pure Nothing
    Right a    -> if anaTop a == PathEnd topEdges
      then do
        surface <- loadInput (dir </> name <.> "musicxml")
        case observeDerivation' (anaDerivation a) of
          Left _err -> do
            putStrLn $ "could not observe trace for " <> name <> ", skipping."
            pure Nothing
          Right trace -> pure $ Just (name, a, trace, surface)
      else do
        putStrLn $ "derivation for " <> name <> " is incomplete, skipping."
        pure Nothing

loadDir :: FilePath -> IO [Piece]
loadDir dir = do
  files <- FP.getDirectoryFiles dir ["*.analysis.json"]
  let getName file = FP.match "*.analysis.json" file >>= listToMaybe
      names = catMaybes $ getName <$> files
  -- print names
  items <- mapM (loadItem dir) names
  pure $ catMaybes items

-- learning
-- --------

learn :: Hyper PVParams -> [Piece] -> IO (Hyper PVParams)
learn = foldM train
 where
  train prior (name, _, trace, _) =
    case getPosterior prior trace sampleDerivation' of
      Nothing -> do
        putStrLn $ "couldn't compute posterior for " <> name <> ", skipping."
        pure prior
      Just post -> do
        -- putStrLn $ "learned from " <> name <> "."
        pure post

-- evaluating
-- ----------

leaveOneOut :: [a] -> [(a, [a])]
leaveOneOut dataset = go dataset [] []
 where
  go []       _    splits = splits
  go (x : xs) done splits = go xs (x : done) ((x, xs <> done) : splits)

derivationLogProb :: Hyper PVParams -> Trace PVParams -> Double
derivationLogProb hyper trace = logp
  where Just (_, logp) = evalTracePredLogP hyper trace sampleDerivation'

countNotes :: Path [a] b -> Int
countNotes (PathEnd notes       ) = length notes
countNotes (Path notes edges rst) = length notes + countNotes rst

predDiff :: Hyper PVParams -> Hyper PVParams -> Piece -> Double
predDiff prior posterior (name, _, trace, _) = predPost - predPrior
 where
  predPrior = derivationLogProb prior trace
  predPost  = derivationLogProb posterior trace

comparePredProb :: Hyper PVParams -> ([Piece], [Piece]) -> IO Double
comparePredProb prior (test, train) = do
  putStrLn $ "testing on " <> show ((\(name, _, _, _) -> name) <$> test)
  posterior <- learn prior train
  let diffs = predDiff prior posterior <$> test
      total = sum diffs
  putStrLn $ "  total Δlogp: " <> show total
  putStrLn $ "  total Δp: " <> show (exp total)
  pure total

sampleBaselines
  :: (StatefulGen g IO) => g -> Hyper PVParams -> Piece -> IO [Double]
sampleBaselines gen posterior (name, _, _, surface) = do
  derivsTry <- replicateM 200 $ runExceptT $ Greedy.parseRandom'
    gen
    protoVoiceEvaluator
    surface
  let derivs = rights derivsTry
  putStrLn $ "  collected " <> show (length derivs) <> " samples for " <> name
  let baselines = flip fmap derivs $ \ana -> do
        trace <- observeDerivation' (anaDerivation ana)
        pure $ derivationLogProb posterior trace
  fmap catMaybes $ forM baselines $ \case
    Left  err -> putStrLn err >> pure Nothing
    Right val -> pure $ Just val

summarize :: [Double] -> (Double, Double)
summarize xs = (Stats.mean sample, Stats.stdDev sample)
  where sample = V.fromList xs

comparePerNote
  :: (StatefulGen g IO)
  => g
  -> Hyper PVParams
  -> (Piece, [Piece])
  -> IO (Double, Double, Double, Double)
comparePerNote gen prior (test@(tstName, _, tstTrace, tstSurface), train) = do
  putStrLn $ "testing on " <> tstName
  posterior <- learn prior train
  -- compute normalized logprobs
  let nnotes      = int2Double $ countNotes tstSurface
      logpPrior   = derivationLogProb prior tstTrace
      logpPost    = derivationLogProb posterior tstTrace
      logppnPrior = logpPrior / nnotes
      logppnPost  = logpPost / nnotes
  baselines <- sampleBaselines gen posterior test
  let (basemean, basestd) = summarize baselines
      basemeanpn          = basemean / nnotes
      basestdpn           = basestd / nnotes
  putStrLn
    $  "  baseline logppn: mean="
    <> show basemeanpn
    <> ", std="
    <> show basestdpn
  putStrLn $ "  nnotes: " <> show nnotes
  putStrLn $ "  logppn prior: " <> show logppnPrior <> " nats"
  putStrLn $ "  logppn posterior: " <> show logppnPost <> " nats"
  putStrLn $ "  perplexity prior: " <> show (exp $ negate logppnPrior)
  putStrLn $ "  perplexity posterior: " <> show (exp $ negate logppnPost)
  putStrLn $ "  Δlogppn: " <> show (logppnPost - logppnPrior) <> " nats"
  pure (logpPrior, logpPost, nnotes, basemean)


-- logpPerNote hyper pieces = sum logs / int2Double (sum counts)
--  where
--   logs   = fmap (\(_, _, trace, _) -> derivationLogProb hyper trace) pieces
--   counts = fmap (\(_, _, _, surface) -> countNotes surface) pieces