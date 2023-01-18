module Heuristics where

import Debug.Trace
import Common
import Data.ByteString.Lazy qualified as BL
import Control.Monad.Except (ExceptT,runExceptT, lift, throwError)
import Data.HashSet qualified as S
import Data.Csv
import Data.List.Split
import Data.Foldable (toList)
import Control.Monad (foldM)
import Data.Hashable
import Internal.MultiSet qualified as MS
import Data.List qualified as L
import Data.Map qualified as M
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
import Evaluator
import HeuristicParser
import HeuristicSearch
import PBHModel
import Language.Haskell.DoNotation
import Musicology.Core qualified as Music
import Musicology.Pitch.Spelled
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

type State ns = SearchState (Edges ns) [Edge ns] (Notes ns) (PVLeftmost ns)

applyHeuristic
  :: ((Maybe (State SPitch), State SPitch) -> ExceptT String IO Double)
  -> (Maybe (State SPitch), State SPitch)
  -> ExceptT String IO Double
applyHeuristic heuristic (prevState, state) = do
   heuristic (prevState, state)
 where
  -- op = case getOpsFromState state of
  --   [] -> Nothing
  --   x : _ -> Just x

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
  lift $ putStrLn "______________________________________________________"
  if isNothing op
    then pure 100.0
    else case fromJust op of
      -- Freezing
      -- no unfreeze if there are 3 open non boundary transition
      LMDouble (LMDoubleFreezeLeft freezeOp) -> do
        lift $ putStrLn $ "Considering an unfreeze"
        lift $ putStrLn $ "Resulting State: " <> show state
        (_ , parent, _, _, _) <- getParentDouble state
        case applyFreeze freezeOp parent of
          Left err -> throwError err
          Right frozen -> pure 8

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
      LMDouble (LMDoubleSpread spreadOp@(SpreadOp spreads edges)) -> do
        lift $ putStrLn $ "Resulting State: " <> show state
        res <- getParentDouble state
        (_,parentl, slc, parentr, _) <- case res of 
          (_,_,Start,_,_) -> throwError "StartStop in middle of spread"
          (_,_,Stop,_,_) -> throwError "StartStop in middle of spread"
          (sl,parentl,Inner slc,parentr,sr) -> pure (sl,parentl,slc,parentr,sr)
        case applySpread spreadOp parentl slc parentr of
          Left err -> throwError err
          Right (childl, slcl, childm, slcr, childr) -> do
            lift $ putStrLn "Considering an unspread"
            lift $ print (slcl,slc,slcr)
            -- lift $ print (parentl, slc, parentr)
            -- lift $ print "parent:"
            let (root, chordType, cProb) = mostLikelyChordFromSlice params slc
            -- lift $ print "left:"
            -- lift $ print $ mostLikelyChordFromSlice params slcl
            -- lift $ print "right:"
            -- let (root, chordType, cProb) = mostLikelyChordFromSlice params ns in 
            --                     pure $ Just (ChordLabel chordType (sic (root - 14)) (spc 0), cProb)

           -- lift $ print $ mostLikelyChordFromSlice params slcr
            let mu = sliceChordLogLikelihood params (mkLbl root chordType) (transformSlice slc)
            let wu = sliceChordLogLikelihood params (mkLbl root chordType) (transformSlice slcl)
            let ru = sliceChordLogLikelihood params (mkLbl root chordType) (transformSlice slcr)
            let score = wu + ru 
            -- lift $ print $ "mu: " <> show mu
            lift $ putStrLn $ "score: " <> show (1 - score)
            pure $ 5 - score 
           where
            -- trace (show $ mostLikelyChordFromSlice params parent ) -2.0

            mkLbl root chordType = ChordLabel chordType (sic (root-14)) (spc 0)
            -- predChord = mostLikelyChord hpData parent
            -- jointLogLikelihood =
            --     sliceChordLogLikelihood childl
            --   + sliceChordLogLikelihood childr
            --   - sliceChordLogLikelihood predChord

            -- jointLikelihood
            go :: SpreadDirection -> Double
            go = undefined

      -- Freezing (Terminate)
      -- numSlices From  SSFrozen with 1 or 1+ transitions
      LMSingle (LMSingleFreeze freezeOp) -> do
        lift $ putStrLn "Considering an unfreeze"
        lift $ putStrLn $ "Resulting State: " <> show state
        (_, parent) <- getParentSingle state
        case applyFreeze freezeOp parent of
          Left err -> throwError err
          Right frozen -> pure 0.2

      -- Splitting Only
      {-
                                      split:
                             ..=[mS]--parent---end
                                  cl\        /cr
                                      [slc]
      -}
      LMSingle (LMSingleSplit splitOp) -> do
        lift $ putStrLn "Considering a single unsplit"
        lift $ putStrLn $ "Resulting State: " <> show state
        (slcl, parent) <- getParentSingle state
        scoreSplit splitOp parent slcl slcl Stop
      -- Splitting Left
      {-
                                      split:
                             ..=[slcl]--parent---[slcr]--_--[_]...
                                  cl\        /cr
                                      [slc]
      -}
      {-
                                      split:
                                ⋊=---parent---[slcr]--_--[_]...
                                  cl\        /cr
                                      [slc]
      -}
      LMDouble (LMDoubleSplitLeft splitOp) -> do
        lift $ putStrLn "Considering an unsplit left"
        lift $ putStrLn $ "Resulting State: " <> show state
        (child, sr) <- getParentSlices (fromJust prevState)
        (slcl, parent, slcr,_, _) <- getParentDouble state
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
        lift $ putStrLn "Considering an unsplit right"
        lift $ putStrLn $ "Resulting State: " <> show state
        (sl, child) <- getParentSlices (fromJust prevState)
        (_,_, slcl, parent, slcr) <- getParentDouble state
        -- (slcl, parent, slcr,_, _) <- getParentDouble state
        scoreSplit splitOp parent child slcl slcr
        -- case applySplit splitOp parent of
        --   Left err -> throwError err
        --   Right (childl, slice, childr) -> do
        --     let thetaL = mostLikelyChordFromSlice params slcl
        --     -- let thetaR = mostLikelyChordFromSlice params slcr
        --     -- lift $ print thetaL
        --     -- lift $ print thetaR
        --     pure 0.4
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


  scoreSplit 
    splitOp@(SplitOp splitRegs splitPassings ls rs keepl keepr passl passr) 
    parent@(Edges topRegs topPassings) 
    childSlice
    slcl 
    slcr = do 
      -- lift $ print $ "lblL"
      lblL <- case slcl of 
                Start -> pure Nothing
                Stop -> throwError "Stop on Left?"
                Inner ns -> let (root, chordType, cProb) = mostLikelyChordFromSlice params ns in 
                                pure $ Just (ChordLabel chordType (sic 0) (spc (root - 14)), cProb)

      -- lift $ print $ "lblR"
      lblR <- case slcr of 
                Start -> throwError "Start on Right?"
                Stop -> pure Nothing
                Inner ns -> let (root, chordType, cProb) = mostLikelyChordFromSlice params ns in 
                                pure $ Just (ChordLabel chordType (sic 0) (spc (root - 14)), cProb)

      
      
      -- let (rootl, chordTypel, cProbl) = mostLikelyChordFromSlice params slcl
      -- let (rootr, chordTyper, cProbr) = mostLikelyChordFromSlice params slcr
      -- Get regular edges - sequential connection , either repitiion or neighbor
      -- notesReg <- applyRegs topRegs splitRegs
      -- Get passing notes - intervals larger than a step
      -- (notesPassing, leftPassings, rightPassings) <- applyPassings topPassings splitPassings




      probsRS <- mapM (scoreLeftOrnament lblR) (allOrnaments rs)  
      probsLS <- mapM (scoreRightOrnament lblL) (allOrnaments ls)  
      probsRegs <- mapM (scoreRegs lblL lblR) (allRegs splitRegs)  
      probsPassings <- mapM (scorePassing lblL lblR) (allPassings splitPassings)  

      -- let notesL = collectNotes ls
      --     notesR = collectNotes rs
      --     notes = MS.unions [notesReg, notesPassing, notesL, notesR]
      -- rs - Maps notes from the right parentSlice to the list of ornaments.


      lift $ putStrLn $ "Child slice: " <> show childSlice
      -- lift $ putStrLn $ "Left slice ornaments:" <> showOps opLs
      lift $ putStrLn $ "Left parent slice: " <> show slcl
      lift $ putStrLn $ "Inferred Chord: " <> showLbl lblL
      lift $ putStrLn $ "Left slice ornaments:" <> showOps opLs
      lift $ putStrLn $ "Right parent slice: " <> show slcr
      lift $ putStrLn $ "Inferred Chord: " <> showLbl lblR
      lift $ putStrLn $ "Right slice ornaments:" <> showOps opRs
      -- lift $ putStrLn $ "child slice: " <> show (getPathFromState state)

      lift $ putStrLn $ "Regular edges: " <> show (toList splitRegs)
      lift $ putStrLn $ "Passing edges: " <> show (toList splitPassings)
      lift $ putStrLn $ "Keep ledges: " <> show (toList keepl)
      lift $ putStrLn $ "Keep redges: " <> show (toList keepr)
      lift $ putStrLn $ "Pass ledges: " <> show (MS.toList passl)
      lift $ putStrLn $ "Pass redges: " <> show (MS.toList passr) 
      -- lift $ putStrLn $ "parent: " <> show parent
      -- lift . print $ Edges keepl (MS.union leftPassings passl)
      -- lift . print $ Edges keepr (MS.union rightPassings passr)
      let aggregateProbs = probsRS <> probsLS <> probsRegs <> probsPassings

      lift $ putStrLn $ "Operation scores: " <> show aggregateProbs

      let score = 1 - (sum aggregateProbs / fromIntegral (L.length aggregateProbs))
      -- let score = 5.5
      
      lift $ putStrLn $ "score: " <> show score
      pure score
         where
          -- showRightOrnament :: (a,(a,RightOrnament))
          -- showRightOrnament (x,(n,orn))= show <> ""

          opLs = showL <$> M.toList ls
          opRs = showR <$> M.toList rs

          showLbl :: Maybe (ChordLabel, Double) -> String
          showLbl Nothing = "N/A"
          showLbl (Just (lbl, prob)) = Music.showNotation (keyCenter lbl) <> chordType lbl <> ", Prob: " <> show prob
          showOps ops = "\n   " <>  L.intercalate "\n   " ops 

          showEdge (p1, p2) = Music.showNotation p1 <> "-" <> Music.showNotation p2
          showEdges ts = "{" <> L.intercalate "," (showEdge <$> toList ts) <> "}"
          showChild (p, o) = Music.showNotation p <> ":" <> show o
          showChildren cs = "[" <> L.intercalate "," (showChild <$> cs) <> "]"

          showSplit (e, cs) = showEdge e <> "=>" <> showChildren cs
          showL (p, lchilds) = Music.showNotation p <> "=>" <> showChildren lchilds
          showR (p, rchilds) = showChildren rchilds <> "<=" <> Music.showNotation p

          scoreRightOrnament Nothing (x,(n,orn)) = throwError "Right Ornament with no parent"
          -- scoreRightOrnament (Just (lbl, prob)) (x,(n,orn)) = pure 0
          scoreRightOrnament (Just (lbl, prob)) (x,(n,orn)) = do
            lift $ putStrLn $ "Scoring a right ornament: " <> Music.showNotation n <> "<="<> Music.showNotation x
            let (x',n') = (transformPitch x,transformPitch n) in case orn of 
              RightNeighbor -> do
                  lift $ putStrLn $ "Left Neighbor" 
                  lift $ putStrLn $ "LBL: " <> (showLbl (Just (lbl, prob)))
                  lift $ putStrLn $ "Chord tone likelihood: " <> (show $ chordToneLogLikelihood params lbl x')
                  lift $ putStrLn $ "Ornament tone likelihood: " <> (show $ ornamentLogLikelihood params lbl n' )
                  pure $ chordToneLogLikelihood params lbl x' + ornamentLogLikelihood params lbl n'
              RightRepeat -> pure $ 1+ chordToneLogLikelihood params lbl x' + chordToneLogLikelihood params lbl n'

          scoreLeftOrnament Nothing (x,(n,orn)) = throwError "Left Ornament with no parent"
          scoreLeftOrnament (Just (lbl, prob)) (x,(n,orn)) = do
            lift $ putStrLn $ "Scoring a left ornament: " <> Music.showNotation x <> "=>"<> Music.showNotation n
            let (x',n') = (transformPitch x,transformPitch n) in case orn of 
              LeftNeighbor -> do 
                lift $ putStrLn $ "Left Neighbor" 
                lift $ putStrLn $ "Chord tone likelihood: " <> (show $ chordToneLogLikelihood params lbl x')
                lift $ putStrLn $ "LBL: " <> (showLbl (Just (lbl, prob)))
                lift $ putStrLn $ "Ornament tone likelihood: " <> (show $ ornamentLogLikelihood params lbl n' )
                pure $ chordToneLogLikelihood params lbl n' + ornamentLogLikelihood params lbl x'
              LeftRepeat -> pure $ 1 + chordToneLogLikelihood params lbl x' + chordToneLogLikelihood params lbl n'

          scoreRegs Nothing _ ((x,y),(n,orn)) = throwError "wtf man"
          scoreRegs _ Nothing ((x,y),(n,orn)) = throwError "wtf woman"
          scoreRegs (Just (lbll, probL)) (Just (lblr, probR)) ((Inner x,Inner y),(n,orn)) =
            let (x',y',n') = (transformPitch x,transformPitch y, transformPitch n) in case orn of 
              FullNeighbor -> pure  $ chordToneLogLikelihoodDouble params lbll lblr x' + ornamentLogLikelihoodDouble params lbll lblr n'
              FullRepeat -> pure $ chordToneLogLikelihoodDouble params lbll lblr x' 
              RootNote -> pure  2.0
              LeftRepeatOfRight -> pure $  chordToneLogLikelihood params lblr y' + ornamentLogLikelihood params lblr n'
              RightRepeatOfLeft -> pure $ chordToneLogLikelihood params lbll x' + ornamentLogLikelihood params lbll n'

          scorePassing Nothing _ ((x,y),(n,orn)) = throwError "wtf man"
          scorePassing _ Nothing ((x,y),(n,orn)) = throwError "wtf man"
          scorePassing (Just (lbll,probl)) (Just (lblr, probr)) ((x,y),(n,orn)) = 
            let (x',y',n') = (transformPitch x,transformPitch y, transformPitch n) in case orn of 
            PassingLeft ->  pure $ chordToneLogLikelihood params lbll x' + ornamentLogLikelihood params lblr n'
            PassingMid ->  pure $ chordToneLogLikelihoodDouble params lbll lblr y' + ornamentLogLikelihoodDouble params lbll lblr n'
            PassingRight ->  pure $ chordToneLogLikelihood params lblr y' + ornamentLogLikelihood params lblr n'

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

          applyPassings top ops = do
            (top', notes, lPassings, rPassings) <-
              foldM applyPassing (top, MS.empty, MS.empty, MS.empty) $ allOps ops
            if MS.null top'
              then pure (notes, lPassings, rPassings)
              else
                throwError $
                  "did not use all non-terminal edges, remaining: "
                    <> showEdges
                      (MS.toList top')

          applyPassing (top, notes, lPassings, rPassings) (parent@(pl, pr), (note, pass))
            | parent `MS.member` top =
                pure (top', notes', lPassings', rPassings')
            | otherwise =
                throwError $
                  "used non-existing non-terminal edge\n  top="
                    <> show parent
                    <> "\n  split="
                    <> show splitOp
           where
            top' = MS.delete parent top
            notes' = MS.insert note notes
            (newl, newr) = case pass of
              PassingMid -> (MS.empty, MS.empty)
              PassingLeft -> (MS.empty, MS.singleton (note, pr))
              PassingRight -> (MS.singleton (pl, note), MS.empty)
            lPassings' = MS.union newl lPassings
            rPassings' = MS.union newr rPassings

          singleChild (_, (note, _)) = note
          collectNotes ops = MS.fromList $ singleChild <$> allOps ops
  getParentDouble state = case state of
    SSFrozen _ -> throwError "Illegal double operation" -- SSFrozen can only be the frist state.
    SSOpen open ops ->
      case open of
        Path tl slice (Path tr sm rst) -> pure (Start, tContent tl, Inner $ sContent slice, tContent tr, Inner $ sContent sm) -- SSOpen only case is a split from  two open transitions.
        Path tl slice (PathEnd tr) -> pure (Start, tContent tl, Inner $ sContent slice, tContent tr, Stop) 
        PathEnd _ -> throwError "illegal double operation" -- SSOpen only case is a split from  two open transitions.
    SSSemiOpen frozen (Slice midSlice) open ops ->
      case open of -- From two open transitions only
        Path tl slice (Path tr sm rst) -> pure (Inner $ midSlice, tContent tl, Inner $ sContent slice, tContent tr, Inner $ sContent sm) -- SSOpen only case is a split from  two open transitions.
        Path tl slice (PathEnd tr) -> pure (Inner $ midSlice, tContent tl, Inner $ sContent slice, tContent tr, Stop) -- SSOpen only case is a split from  two open transitions.
        PathEnd tl -> throwError "Illegal double operation in the SSSemiOpen case"

  getParentSingle state = case state of
    SSFrozen _ -> throwError "Illegal single operation" -- SSFrozen can only be the frist state.
    SSOpen open ops ->
      case open of
        PathEnd parent -> pure $ (Start, tContent parent) -- SSOpen only case is a split from  two open transitions.
        _ -> throwError "Illegal single " -- illegal state
    SSSemiOpen frozen (Slice midSlice) open ops ->
      case open of -- From two open transitions only
        PathEnd parent -> pure $ (Inner midSlice, tContent parent)
        _ -> throwError "Illegal single" -- illegal state
