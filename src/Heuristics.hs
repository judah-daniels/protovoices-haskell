module Heuristics where

-- LOGGING
import Control.Logging qualified as Log
import qualified Data.Text as T

import Common
import PVGrammar hiding
  ( slicesFromFile,
  )
import PVGrammar.Generate
import PVGrammar.Parse
import Internal.MultiSet qualified as MS

import Prelude hiding ( Monad (..), lift, pure,)
import Control.Monad.Except (ExceptT,runExceptT, lift, throwError)
import Language.Haskell.DoNotation
import Control.Monad (foldM)

import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.HashSet qualified as S
import Data.Map qualified as M
import Data.ByteString.Lazy qualified as BL
import Data.Vector qualified as V
import Data.Maybe ( isNothing, fromJust)

import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled

import PBHModel
import HeuristicParser
import HeuristicSearch

type State ns = SearchState (Edges ns) [Edge ns] (Notes ns) (PVLeftmost ns)


applyHeuristic
  :: ((Maybe (State SPitch), State SPitch) -> ExceptT String IO Double)
  -> (Maybe (State SPitch), State SPitch)
  -> ExceptT String IO Double
applyHeuristic heuristic (prevState, state) = do
   heuristic (prevState, state)
 where
  remainingOps :: Double
  remainingOps = fromIntegral $ getPathLengthFromState state

-- ActionDouble (⋊,|{},{F5,C5,A3},|{F5-F5,C5-C5},{F5,C5,G4}) (LMDoubleSplitLeft regular:{}, passing:{}, ls:{}, rs:{[A4:LeftRepeat]<=A3,[C5:LeftRepeat]<=C5,[F3:LeftRepeat,F5:LeftRepeat]<=F5}, kl:{}, kr:{F5-F5,C5-C5}, pl:{}, pr:{})
testOp =  
  ActionDouble 
    (Start, undefined,undefined, undefined, Stop) $
    LMDoubleSplitLeft $ mkSplit $ do 
      addToRight (c' nat) (c' nat) LeftRepeat True 

testHeuristic :: HarmonicProfileData -> (Maybe (State SPitch), State SPitch) -> ExceptT String IO Double
testHeuristic params (prevState, state) = do
  let op = case getOpsFromState state of 
             [] -> Nothing
             (x:_)-> Just x
  -- lift $ putStrLn "______________________________________________________"
  -- lift $ putStrLn $ "Prev State: " <> show prevState
  -- lift $ putStrLn $ "Next State: " <> show state
  --
  if isNothing op
    then pure 0
    else case fromJust op of
      -- Freezing
      LMDouble (LMDoubleFreezeLeft freezeOp) -> pure 0

      LMSingle (LMSingleFreeze freezeOp) -> pure 0  

      -- Spreading
      -- Calculate for each chord possibility, the likelihood that they are all the same chord
      -- Splitting Right
      {-
                              split:
      ..=[_]----pl----[slc]-==----pr---[_]....
            \cl      /     \          /cr
             \      /       \        /
              [slcl]----cm---[ slcr ]
      -}

      LMDouble (LMDoubleSpread spreadOp) -> do
        -- lift $ putStrLn "Considering an unspread"
        res <- getParentDouble (fromJust prevState)
        (_,childl, slcl, childr, slcr) <- case res of 
          (sl,childl,Inner slc,childr,Inner sr) -> pure (sl,childl,slc,childr,sr)
          (_,_,Start,_,_) -> throwError "StartStop left of in spread"
          (_,_,Stop,_,_) -> throwError "StartStop in left of spread"
          (_,_,_,_,Start) -> throwError "StartStop in right of spread"
          (_,_,_,_,Stop) -> throwError "StartStop in right of spread"

        -- pure 30000
        scoreSpread slcl slcr spreadOp 

      -- Freezing (Terminate)
      -- numSlices From  SSFrozen with 1 or 1+ transitions

      -- Splitting Only
      {-
                                      split:
                             ..=[mS]--parent---end
                                  cl\        /cr
                                      [slc]
      -}
      LMSingle (LMSingleSplit splitOp) -> do
        -- lift $ putStrLn "Considering a single unsplit"
        -- (child, sr)
        -- (slcl, parent) <- getParentSingle state
        (slcl, parent, slcr,_, _) <- getParentDouble (fromJust prevState)
        pure 5
        -- scoreSplit splitOp parent slcl slcl Stop

      -- Splitting Left
      {-
                                      split:
                             ..=[slcl]--parent---[slcr]--_--[_]...
                                  cl\        /cr
                                      [slc]
      -}
      LMDouble (LMDoubleSplitLeft splitOp) -> do
        -- lift $ putStrLn "Considering an unsplit left"
        (child, sr) <- getParentSlices (fromJust prevState)
        (slcl, parent, slcr,_, _) <- getParentDouble state
        -- pure 5
        scoreSplit splitOp parent child slcl slcr

      -- Splitting Right
      {-
                                      split:
              ..=[mS]--tl---[slcl]---parent---[slcr]....
                               cl\           /cr
                                  \         /
                                    [ slc ]
                                      split:
              ..=x--tl---[slcl]---parent------x-
                             cl\           /cr
                                \         /
                                  [ slc ]
      -}
      LMDouble (LMDoubleSplitRight splitOp) -> do
        -- lift $ putStrLn "Considering an unsplit right"
        (sl, child) <- getParentSlices (fromJust prevState)
        (_,_, slcl, parent, slcr) <- getParentDouble state
        -- pure 5
        scoreSplit splitOp parent child slcl slcr

 where
      -- Scoring Split
      {-
                                    split:
                         ....[sl]-----parent---[sr]....
                               cl\           /cr
                                  \         /
                                 [ slc child ]
      -}

  getParentSlices :: State ns -> ExceptT String IO (StartStop (Notes ns), StartStop (Notes ns))
  getParentSlices s = do  
    let p = fromJust $ getPathFromState s
    let slices = pathBetweens p
    case slices of 
      (sl:sr:rst) -> pure (Inner sl,Inner sr)
      _ -> throwError "How can you even unsplit if there arent two slices? Confusion"

  scoreSpread
   :: (Music.Notation n) 
   => SliceWrapped (Notes n)
   -> SliceWrapped (Notes n)
   -> Spread SPitch 
   -> ExceptT String IO Double
  scoreSpread (SliceWrapped slcl lblL probL ) (SliceWrapped slcr lblR probR) spreadOp@(SpreadOp spreads edges) = do 
    -- lift $ putStrLn "Considering an unspread"
    let slc = Notes $ MS.fromList $ HM.keys spreads
    -- lift $ print (slcl,slc,slcr)
    -- let wu = sliceChordLogLikelihood params lblL slc
    -- let ru = sliceChordLogLikelihood params lblR slc

    -- let score = 5 - (wu + ru)
    let score = - 4
    -- lift $ putStrLn $ "score: " <> show (1 - score)
    pure $ 1 - score 
   where
    -- trace (show $ mostLikelyChordFromSlice params parent ) -2.0

    mkLbl root chordType = ChordLabel chordType (spc (root-14))
    -- predChord = mostLikelyChord hpData parent
    -- jointLogLikelihood =
    --     sliceChordLogLikelihood childl
    --   + sliceChordLogLikelihood childr
    --   - sliceChordLogLikelihood predChord

    -- jointLikelihood
    go :: SpreadDirection -> Double
    go = undefined

  scoreSplit
   :: (Music.Notation n,  Show a) 
   => Split SPitch 
   -> Edges n
   -> a 
   -> StartStop (SliceWrapped (Notes n))
   -> StartStop (SliceWrapped (Notes n))
   -> ExceptT String IO Double
  scoreSplit' = undefined
  scoreSplit 
    splitOp@(SplitOp splitRegs splitPassings ls rs keepl keepr passl passr) 
    parent@(Edges topRegs topPassings) 
    childSlice
    slcl 
    slcr = do 
      lblL <- case slcl of 
                Start -> pure Nothing
                Stop -> throwError "Stop on Left?"
                Inner (SliceWrapped (Notes _) lbl prob) -> pure $ Just (lbl, prob)
      lblR <- case slcr of 
                Start -> throwError "Start on Right?"
                Stop -> pure Nothing
                Inner (SliceWrapped (Notes _) lbl prob) -> pure $ Just (lbl, prob)

      probsRS <- mapM (scoreLeftOrnament lblR) (allOrnaments rs)  
      probsLS <- mapM (scoreRightOrnament lblL) (allOrnaments ls)  
      probsRegs <- mapM (scoreRegs lblL lblR) (allRegs splitRegs)  
      probsPassings <- mapM (scorePassing lblL lblR) (allPassings splitPassings)  

      -- lift $ putStrLn $ "Left parent slice: " <> show slcl
      -- lift $ putStrLn $ "Inferred Chord: " <> showLbl lblL
      -- lift $ putStrLn $ "Right parent slice: " <> show slcr
      -- lift $ putStrLn $ "Inferred Chord: " <> showLbl lblR
      -- lift $ putStrLn $ "Child slice: " <> show childSlice
      --
      -- lift $ putStrLn $ "Left slice ornaments:" <> showOps opLs 
      -- lift $ putStrLn $ "scores: " <> show probsLS 
      -- lift $ putStrLn $ "Right slice ornaments:" <> showOps opRs
      -- lift $ putStrLn $ "scores: " <> show probsRS 
      --
      -- lift $ putStrLn $ "Regular edges: " <> show (toList splitRegs)
      -- lift $ putStrLn $ "scores: " <> show probsRegs 
      -- lift $ putStrLn $ "Passing edges: " <> show (toList splitPassings)
      -- lift $ putStrLn $ "scores: " <> show probsPassings 
      -- lift $ putStrLn $ "Keep ledges: " <> show (toList keepl)
      -- lift $ putStrLn $ "Keep redges: " <> show (toList keepr)
      -- lift $ putStrLn $ "Pass ledges: " <> show (MS.toList passl)
      -- lift $ putStrLn $ "Pass redges: " <> show (MS.toList passr) 
      --
      let aggregateProbs = probsRS <> probsLS <> probsRegs <> probsPassings

      -- lift $ putStrLn $ "Operation scores: " <> show aggregateProbs

      let score = -1 - (sum aggregateProbs / fromIntegral (L.length aggregateProbs))
      -- lift $ putStrLn $ "score: " <> show score
      pure score
         where
          scoreRightOrnament 
            :: Maybe (ChordLabel, Double) 
            -> (SPitch, (SPitch, RightOrnament)) 
            -> ExceptT String IO Double
          scoreRightOrnament Nothing (n,(x,orn)) = throwError "Right Ornament with no parent"
          scoreRightOrnament (Just (lbl@(ChordLabel chordLbl root), prob)) (n,(x,orn)) = do
            -- lift $ putStrLn ""
            -- lift $ putStrLn $ "Scoring a right ornament: " <> Music.showNotation n <> "<="<> Music.showNotation x
            let (n', x') = (transposeNote root n, transposeNote root x) in case orn of 
              RightNeighbor -> do
                  -- lift $ putStrLn "Right Neighbor" 
                  -- lift $ putStrLn $ "LBL: " <> showLbl (Just (lbl, prob))
                  -- lift $ putStrLn $ "Chord tone likelihood: " <> show (chordToneLogLikelihood params lbl n')
                  -- lift $ putStrLn $ "Ornament tone likelihood: " <> show (ornamentLogLikelihood params lbl x')
                  pure $ chordToneLogLikelihood params lbl n' + ornamentLogLikelihood params lbl x'
              RightRepeat -> do
                -- lift $ putStrLn "Right Repeat" 
                -- lift $ putStrLn $ "LBL: " <> showLbl (Just (lbl, prob))
                -- lift $ putStrLn $ "Chord tone likelihood: " <> show (chordToneLogLikelihood params lbl n')
                pure $ 2 * chordToneLogLikelihood params lbl n' 

          scoreLeftOrnament 
            :: Maybe (ChordLabel, Double) 
            -> (SPitch, (SPitch, LeftOrnament)) 
            -> ExceptT String IO Double
          scoreLeftOrnament Nothing (n,(x,orn)) = throwError "Left Ornament with no parent"
          scoreLeftOrnament (Just (lbl@(ChordLabel _ root), prob)) (n,(x,orn)) = do
            -- lift $ putStrLn $ "Scoring a left ornament: " <> Music.showNotation x <> "=>"<> Music.showNotation n
            let (n', x') = (transposeNote root n, transposeNote root x) in case orn of 
              LeftNeighbor -> do 
                -- lift $ putStrLn "Left Neighbor" 
                -- lift $ putStrLn $ "LBL: " <> showLbl (Just (lbl, prob))
                -- lift $ putStrLn $ "Chord tone likelihood: " <> show (chordToneLogLikelihood params lbl n')
                -- lift $ putStrLn $ "Ornament tone likelihood: " <> show (ornamentLogLikelihood params lbl x')
                pure $ chordToneLogLikelihood params lbl n' + ornamentLogLikelihood params lbl x'  
              LeftRepeat -> do 
                -- lift $ putStrLn "Left Repeat" 
                -- lift $ putStrLn $ "LBL: " <> showLbl (Just (lbl, prob))
                -- lift $ putStrLn $ "Chord tone likelihood: " <> show (chordToneLogLikelihood params lbl n')
                pure $ 2 * chordToneLogLikelihood params lbl n' 

          scoreRegs Nothing _ ((x,y),(n,orn)) = throwError "Attempted to score a regular edge without a chord label"
          scoreRegs _ Nothing ((x,y),(n,orn)) = throwError "Attempted to score a regular edge without a chord label"
          scoreRegs (Just (lbll@(ChordLabel chordlbll rootl), probL)) (Just (lblr@(ChordLabel chordlblr rootr), probR)) ((Inner nl,Inner nr),(x,orn)) = 
            let (nl',nr') = (transposeNote rootl nl,transposeNote rootr nr) in case orn of 
              FullNeighbor -> pure $ 
                (chordToneLogLikelihood params lbll nl' + chordToneLogLikelihood params lblr nr') / 2 
                + ornamentLogLikelihoodDouble params lbll lblr x
              FullRepeat -> pure $ 
                (chordToneLogLikelihood params lbll nl' + chordToneLogLikelihood params lblr nr') / 2 
                + chordToneLogLikelihoodDouble params lbll lblr x 
              RootNote -> pure 0
              LeftRepeatOfRight -> pure $ chordToneLogLikelihood params lblr nr' 
              RightRepeatOfLeft -> pure $ chordToneLogLikelihood params lblr nl'

          scorePassing Nothing _ ((x,y),(n,orn)) = throwError "Attempted to score a passing edge without a chord label"
          scorePassing _ Nothing ((x,y),(n,orn)) = throwError "Attempted to score a passing edge without a chord label"
          scorePassing (Just (lbll@(ChordLabel chordlbll rootl), probL)) (Just (lblr@(ChordLabel chordlblr rootr), probR)) ((nl, nr),(x,orn)) = 
            let (nl',nr') = (transposeNote rootl nl,transposeNote rootr nr) in case orn of 
            PassingLeft ->  pure $ ornamentLogLikelihood params lblr (transposeNote rootl x) + chordToneLogLikelihood params lblr nr'
            PassingMid ->  pure $ ornamentLogLikelihoodDouble params lbll lblr x + (chordToneLogLikelihood params lblr nr' + chordToneLogLikelihood params lblr nl') / 2
            PassingRight ->  pure $ ornamentLogLikelihood params lblr (transposeNote rootr x) + chordToneLogLikelihood params lbll nl'

          opLs = showL <$> M.toList ls
          opRs = showR <$> M.toList rs

          showLbl :: Maybe (ChordLabel, Double) -> String
          showLbl Nothing = "N/A"
          showLbl (Just (lbl, prob)) = Music.showNotation (rootNote lbl) <> chordType lbl <> ", Prob: " <> show prob
          showOps ops = "\n   " <>  L.intercalate "\n   " ops 

          showEdge (p1, p2) = Music.showNotation p1 <> "-" <> Music.showNotation p2
          showEdges ts = "{" <> L.intercalate "," (showEdge <$> S.toList ts) <> "}"
          showChild (p, o) = Music.showNotation p <> ":" <> show o
          showChildren cs = "[" <> L.intercalate "," (showChild <$> cs) <> "]"

          showSplit (e, cs) = showEdge e <> "=>" <> showChildren cs
          showL (p, lchilds) = Music.showNotation p <> "=>" <> showChildren lchilds
          showR (p, rchilds) = showChildren rchilds <> "<=" <> Music.showNotation p

          allOrnaments :: M.Map a [b] -> [(a,b)]
          allOrnaments ornamentSet = do
            (parent, children) <- M.toList ornamentSet
            child <- children
            pure (parent, child)

          allRegs regSet = do
            (parent, children) <- M.toList regSet
            child <- children
            pure (parent, child)

          allPassings ornamentSet = do
            (parent, children) <- M.toList ornamentSet
            child <- children
            pure (parent, child)

          allOps opset = do
            (parent, children) <- M.toList opset
            child <- children
            pure (parent, child)

          applyRegs top ops = do
            (top', notes) <- foldM (applyReg top) (top, MS.empty) $ allOps ops
            if S.null top'
              then pure notes
              else throwError $ "did not use all terminal edges, remaining: " <> showEdges top'

          applyReg topAll (top, notes) (parent, (note, _))
            | parent `S.member` topAll =
                pure (top', notes')
            | otherwise =
                throwError $
                  "used non-existing terminal edge\n  top="
                    <> show parent
                    <> "\n  split="
                    <> show splitOp
           where
            top' = S.delete parent top
            notes' = MS.insert note notes

          singleChild (_, (note, _)) = note
          collectNotes ops = MS.fromList $ singleChild <$> allOps ops

  getParentDouble state = case state of
    SSFrozen _ -> throwError "Illegal double operation" -- SSFrozen can only be the frist state.
    SSOpen open ops ->
      case open of
        Path tl slice (Path tr sm rst) -> pure (Start, tContent tl, Inner slice, tContent tr, Inner sm) -- SSOpen only case is a split from  two open transitions.
        Path tl slice (PathEnd tr) -> pure (Start, tContent tl, Inner slice, tContent tr, Stop) 
        PathEnd _ -> throwError "illegal double operation" -- SSOpen only case is a split from  two open transitions.
    SSSemiOpen frozen midSlice open ops ->
      case open of -- From two open transitions only
        Path tl slice (Path tr sm rst) -> pure (Inner midSlice, tContent tl, Inner slice, tContent tr, Inner sm) -- SSOpen only case is a split from  two open transitions.
        Path tl slice (PathEnd tr) -> pure (Inner midSlice, tContent tl, Inner slice, tContent tr, Stop) -- SSOpen only case is a split from  two open transitions.
        PathEnd tl -> throwError "Illegal double operation in the SSSemiOpen case"

  getParentSingle state = case state of
    SSFrozen _ -> throwError "Illegal single operation" -- SSFrozen can only be the frist state.
    SSOpen open ops ->
      case open of
        PathEnd parent -> pure (Start, tContent parent) -- SSOpen only case is a split from  two open transitions.
        _ -> throwError "Illegal single " -- illegal state
    SSSemiOpen frozen midSlice open ops ->
      case open of -- From two open transitions only
        PathEnd parent -> pure (Inner midSlice, tContent parent)
        _ -> throwError "Illegal single" -- illegal state
