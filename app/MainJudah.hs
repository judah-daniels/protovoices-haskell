{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common

import HeuristicParser
import HeuristicSearch

import PVGrammar hiding
  ( slicesFromFile,
  )
import PVGrammar.Generate
import PVGrammar.Parse


import Display
import Language.Haskell.DoNotation

import Evaluator

import qualified Musicology.Core as Music
import Musicology.Pitch.Spelled 

import Prelude hiding
  ( Monad (..),
    pure,
    lift
  )

import Control.Monad.Except
  ( ExceptT
  , MonadError (throwError), runExceptT
  )
import Control.Monad.IO.Class
  ( MonadIO
  )
import Control.Monad.Trans.Class (lift)
import Data.Maybe
  ( catMaybes
  , mapMaybe
  , fromMaybe
  , maybeToList 
  )
import System.Random (initStdGen)
import System.Random.Stateful
  ( StatefulGen
  , newIOGenM
  , uniformRM
  , randomRIO
  )
import Control.Exception (evaluate)
import Evaluator (ChordLabel(ChordLabel))

-- seed::Int
-- seed = 37
-- generator = mkStdGen seed

-- | The musical surface from Figure 4 as a sequence of slices and transitions.
-- Can be used as an input for parsing.
path321sus =
  Path [e' nat, c' nat] [(Inner $ c' nat, Inner $ c' nat)] $
    Path [d' nat, c' nat] [(Inner $ d' nat, Inner $ d' nat)] $
      Path [d' nat, b' nat] [] $
        PathEnd [c' nat]

testInput :: [ ([(SPC, Bool)], Bool) ]
testInput = 
  [([(e' nat, True), (c' nat, False)], False)
  ,([(e' nat, True)], True)
  ,([(e' nat, False)], False)
  ]


pathFromSlices 
  :: Eval (Edges SPC) [Edge SPC] (Notes SPC) [SPC] (PVLeftmost SPC)
  -> [ ([(SPC, Bool)], Bool)] 
  -> Path (Maybe [Edge SPC], Bool) (Slice (Notes SPC)) 
pathFromSlices eval = reversePath . mkPath Nothing 
  where
    mkPath 
      :: Maybe [Edge SPC] 
      -> [([(SPC, Bool)], Bool)] 
      -> Path (Maybe [Edge SPC], Bool) (Slice (Notes SPC)) 

    mkPath eLeft ((slice,boundary):rst) = Path (eLeft, boundary) (Slice $ evalSlice' (map fst slice)) $ mkPath (Just $ getTiedEdges slice) rst
    mkPath eLeft [] = PathEnd (Nothing, False)
    
    evalSlice' :: [SPC] -> Notes SPC 
    evalSlice' = evalSlice eval

    getTiedEdges :: [(SPC, Bool)] -> [Edge SPC]
    getTiedEdges  = mapMaybe mkTiedEdge 
      where   
        mkTiedEdge :: (SPC, Bool) -> Maybe (Edge SPC)
        mkTiedEdge (_, False) = Nothing
        mkTiedEdge (pitch, True) = Just (Inner pitch, Inner pitch)

-- path321 :: Path
-- t -- s -- t
path321 :: Path (Maybe [Edge SPC], Bool) (Slice (Notes SPC))
path321 =
  Path (Nothing, False) (Slice (evalSlice eval [e' nat, d' nat])) $
    PathEnd (Nothing, False)
  where
    eval :: Eval (Edges SPC) [Edge SPC] (Notes SPC) [SPC] (PVLeftmost SPC)
    eval = protoVoiceEvaluator

main :: IO ()
-- main = print $ pathFromSlices protoVoiceEvaluator testInput
-- main = mainHeuristicSearch
main = mainHeuristicSearch'

-- Testing Heuristic search with a simple search problem
mainHeuristicSearch :: IO ()
mainHeuristicSearch = do 
  print r
    where 
      res = heuristicSearch initState exploreStates goalTest heuristic
      r = fromMaybe 0 res
      initState = 0 :: Float
      exploreStates n = [n+4, n-17, n+30] :: [Float]
      goalTest = (==) (29::Float) 
      heuristic :: Float -> Float
      heuristic x = (x-20) * (x-20) 

slices321sus :: [ ([(SPC, Bool)], Bool) ]
slices321sus = 
  [ ([(e' nat, False), (c' nat, True)], True)
  , ([(d' nat, True), (c' nat, False)], False)
  , ([(d' nat, False), (b' nat, False)], False)
  , ([(c' nat, False)], False) 
  ]

mainHeuristicSearch' :: IO ()
mainHeuristicSearch' = do 
  -- print finalState 
  -- putStrLn "\nAttempting to plot derivation: "
  -- mapM_ print ops
  -- plotDeriv p "testDeriv.tex" ops
  -- putStrLn "done."
  -- evaluate
  hpData <- loadParams "preprocessing/dcml_params.json"

  let res = evalPath p [ChordLabel "MM7" (sic 0) (f' nat)] hpData

  putStrLn "Derivation complete: "
  putStrLn $ "\nFinal Path: " <> show p
  putStrLn $ "\nEvaluation score: " <> show res

    where
      initialState = SSFrozen $ pathFromSlices protoVoiceEvaluator slices321sus

      eval :: Eval (Edges SPC) [Edge SPC] (Notes SPC) [SPC] (PVLeftmost SPC)
      eval = protoVoiceEvaluator

      finalState = fromMaybe initialState (heuristicSearch initialState getNeighboringStates goalTest heuristic)

      ops = getOpsFromState finalState
      p = getPathFromState finalState

      getNeighboringStates = exploreStates eval

      goalTest s = case s of  
        SSSemiOpen {} -> False 
        SSFrozen {} -> False
        SSOpen p _ -> oneChordPerSegment p
          where
            oneChordPerSegment :: Path (Trans es) (Slice ns) -> Bool
            oneChordPerSegment (PathEnd _ ) = True
            oneChordPerSegment (Path tl _ rst) = tBoundary tl && oneChordPerSegment rst

      -- Where the magic happens!
      heuristic x = 3 

plotDeriv initPath fn deriv = mapM_ printStep derivs
  where
    derivs = unfoldDerivation' initPath derivationPlayerPV deriv
    printStep el = do 
      case el of
        Left err -> do
          putStrLn err
        Right g -> do 
          viewGraph fn g
