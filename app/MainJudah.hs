{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import HeuristicParser
import PVGrammar hiding
  ( slicesFromFile,
  )
import PVGrammar.Generate
import PVGrammar.Parse

import Data.Maybe
  ( catMaybes,
    mapMaybe
  )

import Display
import Language.Haskell.DoNotation

import qualified Musicology.Core as Music
import Musicology.Pitch.Spelled 

import Prelude hiding
  ( Monad (..),
    pure,
  )

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


-- We have slices
--  S S S S 
-- First add transitions -
--   Edge when there is a right tie. 
--   First (And last ) transition is Nothing. 
--   transition boundary where segments are.
-- -S-S-S-S-
-- Then reverse for parsing
-- -S-S-S-S-
-- Evaluate slices
-- -s-s-s-s-
pathFromSlices 
  :: Eval (Edges SPC) [Edge SPC] (Notes SPC) [SPC] (PVLeftmost SPC)
  -> [ ([(SPC, Bool)], Bool)] 
  -> Path (Maybe [Edge SPC], Bool) (Slice (Notes SPC)) 
pathFromSlices eval = reversePath . mkPath (Nothing, False) 
  where
    mkPath 
      :: (Maybe [Edge SPC], Bool) 
      -> [([(SPC, Bool)], Bool)] 
      -> Path (Maybe [Edge SPC], Bool) (Slice (Notes SPC)) 

    mkPath eLeft ((slice,boundary):rst) = Path eLeft (Slice $ evalSlice' (map fst slice)) $ mkPath (Just $ getTiedEdges slice, boundary) rst
    mkPath eLeft [] = PathEnd (Nothing, False)
    
    evalSlice' :: [SPC] -> Notes SPC 
    evalSlice' = evalSlice eval

    getTiedEdges :: [(SPC, Bool)] -> [Edge SPC]
    getTiedEdges  = mapMaybe mkTiedEdge 
      where   
        mkTiedEdge :: (SPC, Bool) -> Maybe (Edge SPC)
        mkTiedEdge (_, False) = Nothing
        mkTiedEdge (pitch, True) = Just (Inner pitch, Inner pitch)
    
    

  -- I   I  - 
  -- e - e  e
  -- c


-- path321 :: Path
-- t -- s -- t
path321 :: Path (Maybe [Edge SPC], Bool) (Slice (Notes SPC))
path321 =
  Path (Nothing, False) (Slice (evalSlice eval [e' nat, d' nat])) $
    PathEnd (Nothing, False)
  where
    eval :: Eval (Edges SPC) [Edge SPC] (Notes SPC) [SPC] (PVLeftmost SPC)
    eval = protoVoiceEvaluator

-- Path (Nothing, False) Start $
--   Path (Just [(e' nat, c' nat)], False) (Inner [c' nat]) $
--     Path (Just [(e' nat, c' nat)], False) Stop $ PathEnd (Nothing, False)

main :: IO ()
-- main = print $ pathFromSlices protoVoiceEvaluator testInput
main = mainParseStep

mainParseStep :: IO ()
mainParseStep = do
  putStrLn "Input:"
  print path321
  putStrLn "Attempting to perform a single parse step\n"
  print $ states
  state <- search' 4 eval s
  print state
  print "done"
  where
    -- print path321
    -- let s = initialState protoVoiceEvaluator  in

    -- s = undefined
    -- s = SSFrozen path321
    s = SSFrozen $ pathFromSlices protoVoiceEvaluator testInput

    eval :: Eval (Edges SPC) [Edge SPC] (Notes SPC) [SPC] (PVLeftmost SPC)
    eval = protoVoiceEvaluator
    states = exploreStates s eval

search' :: forall es es' ns ns' o . Int -> Eval es es' ns ns' o -> SearchState es es' ns o-> IO (SearchState es es' ns o)
search' 0 eval state = pure state
search' i eval state = do 
  -- print nextState
  search' (i-1) eval nextState
  where
    nextState = head $ exploreStates state eval

testState =
  SSFrozen $
    Path (Nothing, False) (Slice [c' nat]) $
      PathEnd (Just [(c' nat, c' nat)], False)

-- = SSFrozen !(Path (Maybe es', Bool) (Slice ns)) -- Beginning of search - all frozen edges and slices
-- initialState :: forall es es' ns ns' o. Eval es es' ns ns' o -> Path es' ns' -> SearchState es es' ns o
-- initialState eval path = SSFrozen $ PathEnd (Just undefined, False)

plotDeriv fn deriv = do
  case replayDerivation derivationPlayerPV deriv of
    (Left err) -> putStrLn err
    (Right g) -> viewGraph fn g
