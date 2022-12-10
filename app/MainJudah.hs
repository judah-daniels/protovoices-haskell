{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import System.Random
import Common
import HeuristicParser
import HeuristicSearch
import PVGrammar hiding
  ( slicesFromFile,
  )
import PVGrammar.Generate
import PVGrammar.Parse

import Data.Maybe
  ( catMaybes,
    mapMaybe,
    fromMaybe
  )

import Display
import Language.Haskell.DoNotation

import qualified Musicology.Core as Music
import Musicology.Pitch.Spelled 

import Prelude hiding
  ( Monad (..),
    pure,
  )

seed::Int
seed = 37
generator = mkStdGen seed

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


mainHeuristicSearch :: IO ()
mainHeuristicSearch = do 
  print r
    where 
      res = heuristicSearch'' initState exploreStates goalTest heuristic
      r = fromMaybe 0 res
      initState = 0 :: Float
      exploreStates n = [n+4, n-17, n+30] :: [Float]
      goalTest = (==) (29::Float) 
      heuristic :: Float -> Float
      heuristic x = (x-20) * (x-20) 

mainParseStep :: IO ()
mainParseStep = do
  (state, ops, p) <- search' 9 eval s

  print state
  putStrLn "\nAttempting to plot derivation: "
  mapM_ print ops
  plotDeriv p "testDeriv.tex" ops

  putStrLn "done."
  -- print statee
  where
    s = SSFrozen $ pathFromSlices protoVoiceEvaluator slices321sus

    eval :: Eval (Edges SPC) [Edge SPC] (Notes SPC) [SPC] (PVLeftmost SPC)
    eval = protoVoiceEvaluator
    -- states = exploreStates eval s 
    --
    -- state' = heuristicSearch s (exploreStates eval) isGoalState heuristic
    --   where
    --     isGoalState x = True 
    --     heuristic x = 1

slices321sus :: [ ([(SPC, Bool)], Bool) ]
slices321sus = 
  [ ([(e' nat, False), (c' nat, True)], True)
  , ([(d' nat, True), (c' nat, False)], False)
  , ([(d' nat, False), (b' nat, False)], False)
  , ([(c' nat, False)], False) 
  ]

getOpsFromState 
  :: SearchState es es' ns o
  -> [o]
getOpsFromState s = case s of 
  SSOpen p d -> d
  SSSemiOpen p m f d -> d 
  SSFrozen p -> []

getPathFromState
  :: SearchState es es' ns o
  -> Path es ns
getPathFromState s = case s of
  SSOpen p d -> transformPath p
  SSSemiOpen p m f d -> undefined 
  SSFrozen p -> undefined
  where
    transformPath
      :: Path (Trans es) (Slice ns)
      -> Path es ns
    transformPath (PathEnd t) = PathEnd (tContent t)
    transformPath (Path t s rst) = Path (tContent t) (sContent s) $ transformPath rst

search' 
  :: forall es es' ns ns' o 
  .  (Show es, Show es', Show ns, Show ns', Show o) 
  => Int 
  -> Eval es es' ns ns' o 
  -> SearchState es es' ns o
  -> IO (SearchState es es' ns o, [o], Path es ns)
search' 0 eval state = pure (state, getOpsFromState state, getPathFromState state)

search' i eval state = do 
  -- print state
  search' (i-1) eval nextState
  where
    nextStates = (exploreStates eval state) 
    n = length nextStates
    (rand, _) =randomR (0, (n-1)) generator
    nextState = case n of 
      0 -> state 
      _ -> nextStates !! rand


mainHeuristicSearch' :: IO ()
mainHeuristicSearch' = do 
  print finalState 

  putStrLn "\nAttempting to plot derivation: "

  mapM_ print ops
  plotDeriv p "testDeriv.tex" ops

  putStrLn "done."
    where
      initialState = SSFrozen $ pathFromSlices protoVoiceEvaluator slices321sus

      eval :: Eval (Edges SPC) [Edge SPC] (Notes SPC) [SPC] (PVLeftmost SPC)
      eval = protoVoiceEvaluator

      finalState = fromMaybe initialState (heuristicSearch'' initialState getNeighboringStates goalTest heuristic)

      ops = getOpsFromState finalState
      p = getPathFromState finalState

      getNeighboringStates = exploreStates eval

      goalTest s = case s of  
        SSOpen p d -> True
        SSSemiOpen p m f d -> False  
        SSFrozen p -> False


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
