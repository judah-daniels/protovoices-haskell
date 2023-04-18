module Probability
  ( multinomialLogProb
  , categoricalLogProb
  , poissonSample
  , reservoirSample
  , pickRandom
  )
where

import qualified Data.Vector as V
import Numeric.SpecFunctions (logGamma)
import System.Random.Stateful

multinomialLogProb :: V.Vector Double -> V.Vector Double -> Double
multinomialLogProb xs probs
  | n == 0 = -100000
  | V.length xs /= V.length probs = -100000
  | otherwise = logFactorialN + logPowers
 where
  n = sum xs
  logFactorialN = logGamma $ n + 1
  logPowers = V.sum $ V.zipWith powers xs probs
   where
    powers x y = x * log y - logGamma (x + 1)

-- Calculates the probability density of a multinomial distribution at the given point
categoricalLogProb :: Int -> V.Vector Double -> Double
categoricalLogProb x probs
  | x >= 0 && x < V.length probs = log $ probs V.! x
  | otherwise = -1000000

-- With a simple list, this is pretty instant as long as the total list is less than 1000000
-- Cost is based on size of list rather than reservoir size.
reservoirSample :: StatefulGen g IO => g -> Int -> [a] -> IO [a]
reservoirSample gen k xs
  | n <= k = pure xs
  | otherwise = do
      let res = V.fromList $ take k xs
      r <- uniformRM (0 :: Double, 1 :: Double) gen
      let w = exp (log r / fromIntegral (k - 1))
      z <- go gen k res w (drop k xs)
      pure $ V.toList z
 where
  -- go :: g -> Int -> V.Vector t -> Double -> [t] -> V.Vector t
  go gen i res w [] = pure res
  go gen i res w xs = do
    r <- uniformRM (0, 1) gen
    let jump = floor (logBase (1 - w) r) + 1
    if i + jump > n - 1
      then pure res
      else case drop jump xs of
        [] -> pure res
        xss@(x : _) -> do
          let xss = drop jump xs
          rint <- uniformRM (0, k - 1) gen
          r <- uniformRM (0, 1) gen
          let newres = res V.// [(rint, x)]
          let w' = w * exp (log r / fromIntegral k)
          go gen (i + jump) newres w' xss
  n = length xs
pickRandom :: StatefulGen g m => g -> [a] -> m (Maybe a)
pickRandom _ [] = pure Nothing
pickRandom gen xs = do
  i <- uniformRM (0, length xs - 1) gen
  pure $ Just $ xs !! i

poissonSample :: StatefulGen g IO => g -> Double -> IO Int
poissonSample g lambda = do
  let p = exp (-lambda)
      go k prod = do
        u <- uniformRM (0.0, 1.0) g
        let prod' = prod * u
        if prod' < p
          then return k
          else go (k + 1) prod'
  go 0 1.0
