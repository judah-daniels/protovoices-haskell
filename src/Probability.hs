{-# LANGUAGE UndecidableInstances #-}

module Probability
  ( multinomialLogProb
  , sampleR
  , categoricalLogProb
  , pickRandomChoice
  , pickReservoir10
  , pickRandomN
  , collectRandomSample
  , collectRandomChoice
  , mixture
  -- , ProbTIO
  -- , ProbT
  -- , pickRandom'
  , Prob
  , RandomChoice
  , testProb
  , poissonSample
  , sample
  , reservoirSample
  , pickRandom
  )
where

import Control.Monad
import Control.Monad.Trans.Maybe
import qualified Data.Vector as V
import Debug.Trace

-- import Numeric.Probability.Distribution as P

import Control.Monad.Trans (lift)
import Data.Maybe
import Numeric.SpecFunctions (logGamma)
import System.Random as R
import System.Random.Stateful

{- | Sample n elements from a list
sample :: Show a => Int -> [a] -> Reservoir a [a]
sample n xs = do
  let len = length xs
  let updateReservoir x = do
        ys <- get
        i <- lift $ randomRIO (0, len)
        if i < n
          then put (take i ys ++ [x] ++ drop (i + 1) ys)
          else return ()
  liftIO $ putStrLn "Reservoir sampling..."
  liftIO $ putStrLn $ "List length: " ++ show len
  liftIO $ putStrLn $ "Reservoir size: " ++ show n
  liftIO $ putStrLn "Reservoir:"
  liftIO $ putStrLn "---------"
  put (take n xs)
  forM_ (drop n xs) $ \x -> do
    updateReservoir x
    get >>= liftIO . putStrLn . show
  get
-}

-- testProb i = do
-- x <- YesProb $ 5
-- -- y <- [1, 2, 3]
-- pure $ x
--
-- testProb :: Prob Int
-- testProb = do
--   x <- YesProb [1]
--   y <- YesProb [x + 2]
--   return x
--
-- testtest :: IO (Prob Int)
-- testtest = do
--   gen <- initStdGen
--   mgen <- newIOGenM gen
--   pure $ (sample mgen $ testProb 8)

-- data Prob a b = Prob [a] Int
-- instance RandomChoice (Prob Int) where
--   sample gen [] = NoProb
--   sample gen (x : xs) = YesProb x
-- testtest :: StatefulGen g Prob => g -> Prob Int
-- testtest mgen = do
--   x <- sample mgen [1, 2, 3]
--   y <- sample mgen [4, 5, 6]
--   pure $ x + y

-- testProb :: IO ()
-- testProb = do
--   gen <- initStdGen
--   mgen <- newIOGenM gen
--   print $ runRandomChoice mgen (testtest mgen)

-- testtest :: StdGen -> Prob' Int
-- testtest gen = do
--   x <- sample gen [1 .. 10]
--   y <- sample gen [1 .. 10]
--   a <- sample gen [4, 5, 6]
--   s <- sample gen [4, 5, 6]
--   d <- sample gen [4, 5, 6]
--   pure $ x * y * a * s * d

-- mgen <- newIOGenM gen
-- print $ runRandomChoice (testtest gen)

newtype Prob a = Prob a

newtype RandomChoice a = RandomChoice
  { sample :: StdGen -> [a] -> Prob a
  }

pickReservoir10 :: StatefulGen g IO => g -> [slc] -> MaybeT IO [slc]
pickReservoir10 gen xs = do
  guard . not $ null xs
  lift $ sampleR gen 10 xs

pickRandomChoice :: StatefulGen g IO => g -> [slc] -> MaybeT IO slc
pickRandomChoice gen xs = do
  guard . not $ null xs
  x <- lift $ uniformRM (0, length xs - 1) gen
  return $ xs !! x

-- With a simple list, this is pretty instant as long as the total list is less than 1000000
-- Cost is based on size of list rather than reservoir size.
sampleR :: StatefulGen g IO => g -> Int -> [a] -> IO [a]
sampleR gen k xs
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

-- runRVar dist StdRandom

-- testtest :: IO ()
-- testtest = do
--   let c = oneOf [1, 2, 3, 4]
--   print $ "hi"

-- pickOne :: StatefulGen g IO => g -> ProbTMIO Int
-- pickOne = undefined

-- choice <-

-- pickRandom'' gen xs = do
--   i <- lift $ uniformRM (0, length xs - 1) gen
--   pure $ xs !! i

-- testtest' :: StatefulGen g IO => g -> ProbTMIO Int
-- testtest' g = do
--   x <- undefined
--   pure undefined

-- extract $ pickRandom' [1, 2, 3, 4]
-- t
testtest :: StatefulGen g IO => g -> Int -> MaybeT IO Int
testtest g i = pickRandomChoice g [1 .. i]

testtest' :: StatefulGen g IO => g -> Int -> MaybeT IO Int
testtest' g i = pickRandomChoice g [10 .. (10 + i)]

abc mgen = do
  j <- pickRandomChoice mgen [1 .. 10000]
  k <- testtest mgen j
  l <- testtest mgen k
  pickRandomChoice mgen [1 .. j + k + l]

testProb :: IO ()
testProb = do
  gen <- initStdGen
  mgen <- newIOGenM gen
  x <- collectRandomChoice mgen $ do
    j <- pickRandomChoice mgen [1 .. 10000]
    k <- testtest mgen j
    testtest mgen k

  j <- runMaybeT $ abc mgen

  y <- runMaybeT $ pickRandomChoice mgen ([] :: [Int])
  xs <- runMaybeT $ pickReservoir10 mgen [1 .. 1000000]
  ys <- runMaybeT $ pickReservoir10 mgen ([] :: [Int])
  print j
  print x
  print y
  print xs
  print ys

collectRandomSample :: StatefulGen g IO => g -> MaybeT IO [a] -> IO [a]
collectRandomSample mgen prob = do
  res <- runMaybeT prob
  case res of
    Just a -> pure a
    Nothing -> pure []

collectRandomChoice :: StatefulGen g IO => g -> MaybeT IO a -> IO [a]
collectRandomChoice mgen prob = do
  res <- runMaybeT prob
  case res of
    Just a -> pure [a]
    Nothing -> pure []

-- collectRandomChoice :: StatefulGen g IO => g -> MaybeT IO a -> IO [a]
-- collectRandomChoice mgen prob = do
--   res <- runMaybeT prob
--   case res of
--     Just a -> pure [a]
--     Nothing -> pure []

--   Just a -> pure [a]

-- res <- pickRandom' mgen [1 .. 10000000]
-- -- res <- pickRandom mgen [1 .. 10000000]
-- print $ extract $ res

-- print $ res

-- runRandomChoice gen testtest

-- runRandomChoice :: StdGen -> Prob' a -> a
-- runRandomChoice g pa =

-- instance Functor RandomChoice where
--   fmap :: (a -> b) -> RandomChoice a -> RandomChoice b
--   fmap f (RandomChoice sample) = RandomChoice (\g (b : bs) -> Prob' b)
--
-- instance Applicative RandomChoice where
--   pure :: a -> RandomChoice a
--   pure a = RandomChoice doSample
--    where
--     -- gen = mkStdGen 10
--     doSample :: StdGen -> [a] -> Prob' a
--     doSample gen (x : xs) = Prob' x
--
--   (<*>) :: RandomChoice (a -> b) -> RandomChoice a -> RandomChoice b
--   RandomChoice sample <*> RandomChoice sample' =
--     RandomChoice (\g (b : bs) -> Prob' b)
--
-- RandomChoice (\gen xs ->
--   Prob' b)

--  sample gen (x : xs) = (x, gen)

--
-- instance Monad RandomChoice where
--   (>>=) :: RandomChoice a -> (a -> RandomChoice b) -> RandomChoice b
--   RandomChoice sample >>= f = RandomChoice (\g (b : bs) -> Prob' b)

-- where
--   (p, g) = f ()

-- sample :: (Show a) => StdGen -> [a] -> Prob' a
-- sample gen [] = undefined
-- sample gen xs =
--   let (r, g) = uniformR (0 :: Int, length xs - 1 :: Int) gen
--    in trace (show xs) $ Prob' (xs !! r) g

-- runRandomChoice :: Prob' a -> [a]
-- runRandomChoice (YesProb a) = [a]
-- runRandomChoice gen NoProb = []
-- runRandomChoice (Prob' a g) = [a]

--
-- instance Functor RandomChoice where
--   fmap :: (a -> b) -> RandomChoice a -> RandomChoice b
--   fmap f (RandomChoice a) = RandomChoice $ f a
--
-- instance Applicative RandomChoice where
--   pure :: a -> RandomChoice a
--   pure = RandomChoice
--   (<*>) :: RandomChoice (a -> b) -> RandomChoice a -> RandomChoice b
--   RandomChoice f <*> RandomChoice a = RandomChoice (f a)

-- let (r, g) = uniformR (0 :: Int, length xs - 1 :: Int) gen
-- in
-- Prob' (xs !! r) g
-- f a
-- sample :: (Show a) => StdGen -> [a] -> Prob' a
-- sample gen [] = undefined
-- sample gen xs =

-- sample :: StatefulGen
-- sample gen [] = NoProb
-- sample gen (x : xs) = YesProb x

-- instance Monad RandomChoice

-- instance Functor Prob' where
-- fmap :: (a -> b) -> Prob a -> Prob b
-- fmap f NoProb = NoProb
-- fmap f (Prob' a) = Prob' (f a)

-- instance Applicative Prob' where
-- NoProb <*> _ = NoProb
-- _ <*> NoProb = NoProb
-- YesProb a <*> YesProb b = YesProb a
-- pure :: a -> Prob' a
-- pure a = Prob' a

-- (<*>) :: Prob (a -> b) -> Prob a -> Prob b
-- NoProb <*> _ = NoProb
-- _ <*> NoProb = NoProb
--   Prob' f <*> Prob' x = Prob' (f x)
--
-- instance Monad Prob' where
--   -- NoProb >>= k = NoProb
--   Prob' x >>= k = k x

--
--

{- | Computes the multinomial log probability density function parameterised by prob, evaluated at the vector xs.
The sum of the observations for all categories must add up to the number of samples, n.
  The probabilties must be NORMALISED!
-}
multinomialLogProb :: V.Vector Double -> V.Vector Double -> Double
multinomialLogProb xs probs
  | n == 0 = -100
  | V.length xs /= V.length probs = -100
  | otherwise = logFactorialN + logPowers
 where
  n = sum xs
  logFactorialN = logGamma $ n + 1
  logPowers = V.sum $ V.zipWith powers xs probs
   where
    powers x y = x * log y - logGamma (x + 1)

-- Calculates the probability density of a multinomial distribution at the given point
categoricalLogProb :: Int -> V.Vector Double -> Maybe Double
categoricalLogProb x probs
  | x >= 0 && x < V.length probs = pure $ log $ probs V.! x
  | otherwise = Nothing

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

pickRandom' :: StatefulGen g m => g -> [a] -> m a
pickRandom' _ [] = error "unsafe pickRandom'"
pickRandom' gen xs = do
  i <- uniformRM (0, length xs - 1) gen
  pure $ xs !! i

pickRandomN :: StatefulGen g m => g -> Int -> [a] -> m [a]
pickRandomN _ _ [] = pure []
pickRandomN gen 0 xs = pure []
pickRandomN gen n xs = replicateM n (pickRandom' gen xs)

-- i <- uniformRM (0, length xs - 1) gen
-- pure $ Just $ xs !! i

-- Takes the average of two profiles
mixture :: Maybe (V.Vector Double) -> Maybe (V.Vector Double) -> Maybe (V.Vector Double)
mixture v1m v2m = do
  v1 <- v1m
  v2 <- v2m
  if V.length v1 == V.length v2 then Just $ (/ 2) <$> V.zipWith (+) v1 v2 else Nothing

-- \| otherwise = Nothing

-- Takes the average of two profiles
prodExperts :: V.Vector Double -> V.Vector Double -> Maybe (V.Vector Double)
prodExperts v1 v2
  | V.length v1 == V.length v2 =
      let unNormalised = V.zipWith (*) v1 v2
       in Just $ V.map (/ V.sum unNormalised) unNormalised
  | otherwise = Nothing

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
