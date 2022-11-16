{-# LANGUAGE ScopedTypeVariables #-}

module HeuristicSearch where

import Common
import Control.Applicative
import HeuristicParser
import PVGrammar
import PVGrammar.Parse

-- pathFromSlices
--   :: Eval (Edges SPC) [Edge SPC] (Notes SPC) [SPC] (PVLeftmost SPC)
--   -> [ ([(SPC, Bool)], Bool)]
--   -> Path (Maybe [Edge SPC], Bool) (Slice (Notes SPC))

-- | Entry point to the search algorithm
heuristicSearch ::
  forall state.
  state ->
  (state -> [state]) ->
  (state -> Bool) ->
  (state -> Float) ->
  state
heuristicSearch initialState getNextStates isGoalState heuristic = head nextStates
  where
    nextStates = getNextStates initialState
