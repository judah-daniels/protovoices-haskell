{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FileHandlingSpec where

import Musicology.Core
import Test.Hspec

import Debug.Trace
import Common hiding (split)
import Data.ByteString.Lazy qualified as BL
import Control.Monad.Except (ExceptT,runExceptT, lift, throwError)
import Data.Csv
import Data.Bifunctor (bimap, first, second)
import Data.List.Split
import Data.Hashable
import Data.Maybe
  ( catMaybes,
    isNothing,
    fromMaybe,
    fromJust,
    mapMaybe,
    maybeToList,
  )
import Data.Vector qualified as V
import Display
import RandomChoiceSearch
import RandomSampleParser
import HeuristicSearch
import PBHModel
import Language.Haskell.DoNotation
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled
import Heuristics
import PVGrammar hiding
  ( slicesFromFile,
  )
import PVGrammar.Generate
import PVGrammar.Parse
import Prelude hiding
  ( Monad (..),
    lift,
    pure,
  )
import Control.Monad.State (evalState)
import Control.Monad.Trans.Except (throwE)
import qualified Internal.MultiSet as MS
import HeuristicParser 
import FileHandling
import Control.Logging qualified as Log


fileHandlingSpec :: Spec 
fileHandlingSpec = do
  runIO $ Log.withStdoutLogging $ do 
    lbls <- chordsFromFile "preprocessing/inputs/chords/n14op131_03.csv"
    print lbls
    pure ()

