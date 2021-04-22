{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Parser
import           PVGrammar
import           PVGrammar.Parse
import           PVGrammar.Generate
import           Common
import           Display
import           Scoring

--import Musicology.Internal.Helpers
import           Musicology.MusicXML
import           Musicology.Core
import           Musicology.Core.Slicing
import           Musicology.Pitch.Spelled      as MT

import           Data.Ratio                     ( Ratio(..) )
import           Lens.Micro                     ( over )
import           Data.Maybe                     ( catMaybes )
import           Data.Either                    ( partitionEithers )

import qualified Data.MultiSet                 as MS
import qualified Data.Set                      as S
import qualified Data.Semiring                 as R
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Control.Monad                  ( forM
                                                , forM_
                                                )
import           Data.Typeable                  ( Proxy(Proxy) )

plotSteps fn deriv = do
  let graphs          = unfoldDerivation derivationPlayerNull deriv
      (errors, steps) = partitionEithers graphs
  mapM_ putStrLn errors
  writeGraphs fn $ reverse steps

putGraph n deriv = case replayDerivation' n derivationPlayerNull deriv of
  (Left error) -> putStrLn error
  (Right g) -> T.putStrLn $ mkTikzPic $ tikzDerivationGraph showTex showTex g

example1 =
  buildDerivation
    $  split ()
    .> split ()
    .> freeze ()
    .> hori ()
    .> freeze ()
    .> freeze ()
    .> freeze ()

horiSplitLeft = buildPartialDerivation (Proxy :: Proxy 2) $ hori () .> split ()
splitLeftHori =
  buildPartialDerivation (Proxy :: Proxy 2) $ split () .> freeze () .> hori ()

main = pure ()
