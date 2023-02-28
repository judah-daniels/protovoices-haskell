{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- {-# LANGUAGE RebindableSyntax #-}
module Main where

import Streamly.Prelude qualified as ST
import Streamly.Data.Array.Foreign qualified as STA


import Data.Function ((&))

import Streamly.Prelude ((|:), (|&))

import qualified Streamly.Prelude as Stream

import qualified Streamly.Data.Fold as Fold

import Control.Concurrent (threadDelay, myThreadId)

foreign export ccall triple :: Int -> Int

triple :: Int -> Int
triple x = 3*x

p n = threadDelay (n * 1000000) >> return n

main :: IO ()
main = do
  Stream.repeatM (p 1)
  & Stream.fromSerial   -- repeatM is serial
  & Stream.mapM (\x -> p 1 >> print x)
  & Stream.fromAhead    -- mapM is cocnurrent using Ahead style
  & Stream.drain

  -- jeff <- ST.toList $ ST.mapM myf $ ST.fromList [0 .. 10000000] 
  -- putStrLn $ show $ length jeff


myf :: Int -> IO Int
myf x = pure $ x + 2

