{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module ParserViz where

import ParserOld
import PVGrammar
import Musicology.Core

import qualified Algebra.Graph.Class as G
import qualified Algebra.Graph.AdjacencyMap as GA

import Data.Default
import Data.Foldable (find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Diagrams.Prelude hiding ((^-^), (^+^))
import qualified Diagrams.TwoD.Text
import Diagrams.Backend.SVG as SVG
import Data.Typeable (Typeable)
import Data.Colour

type PVDiagram b = QDiagram b V2 Double Any

epflLight :: Colour Double
epflLight = sRGB24read "#f0f0f0"

pitchNames = ["c","c#/db","d","d#/eb","e","f","f#/gb","g","g#/ab","a","a#/bb","b"]

instance IsName a => IsName (StartStop a)
instance IsName SIC

nodeName (PVNode p f t) = (toInterval <$> p,f,t)

-- node :: (IsName i) => PVNode i -> PVDiagram b
node v@(PVNode p f t) = text (show p)
               # fontSizeL 0.5
               # fc cText
               <> circle 0.7
               # fc cNode
               # named (nodeName v)
               # lw 1.5
  where (cNode,cText) = (epflLight,black)

data GraphOpts i b = GraphOpts
  { goEdgeOpts :: (PVNode i, PVNode i) -> ArrowOpts Double -> ArrowOpts Double
  }

instance Default (GraphOpts i b) where
  def = GraphOpts (const id)

graphDiag' :: (ToMidi i, Interval i, Ord i, IsName i, Show (Pitch i),
               Renderable (Path V2 Double) b,
               Renderable (Diagrams.TwoD.Text.Text Double) b) =>
              PVGraph i -> GraphOpts i b -> PVDiagram b
graphDiag' (PVGraph graph ntgraph _) opts = centerXY $
                        (atPoints (map (mkP2 0 . ptoy) [minp..maxp]) (map pitch pnames)
                         <> rect 1 1 # lw 0)
                        |||
                        (innerNodes
                          # applyAll (map (conn False) tes)
                          # applyAll (map (conn True) ntes)
                         <> mconcat (map (gridy . ptoy) [minp..maxp])
                         <> mconcat (map (gridx . itox . fromIntegral)
                                      [ceiling (minx/10) .. floor (maxx/10)]))
                        -- <> mconcat (map timeslot $ timeslots annot)
                        -- <> rect 2 1 # lw 0 # translateX 5
  where vs = GA.vertexList graph
        ps = toMidi . toInterval <$> onlyInner (pvnPitch <$> vs)
        minp = minimum ps
        maxp = maximum ps
        ptoy p = fromIntegral (p-minp)
        midy = (ptoy minp + ptoy maxp) / 2
        ptoy' (Inner p) = ptoy $ toMidi $ toInterval p
        ptoy' _ = midy
        itox = (*10) . fromIntegral
        xs = map (itox . pvnFirst) vs
        ys = map (ptoy' . pvnPitch) vs
        minx = minimum xs - 1
        maxx = maximum xs + 1
        tes = GA.edgeList graph
        ntes = GA.edgeList ntgraph
        pnames = take (maxp-minp+1) $ drop minp (cycle pitchNames)
        -- start = maybe (PVNode (:⋊) 0 0) id $ find (isStart . pvnPitch) vs
        -- end   = maybe (PVNode (:⋉) 0 0) id $ find (isEnd   . pvnPitch) vs
        aopts nt v1 v2 = with
                & headLength .~ 12
                & goEdgeOpts opts (v1,v2) -- overrides everything else
                & shaftStyle %~ lw 1
                & shaftStyle %~ (if nt then dashingG [0.3, 0.2] 0 else id)
        conn nt (v1, v2) = connectOutside' (aopts nt v1 v2) (nodeName v1) (nodeName v2)
        gridy y = mkP2 minx y ~~ mkP2 maxx y
                  # lw 1
                  # lc lightgrey -- (if y `elem` [0,2,4,5,7,9,11] then grey else  lightgrey)
        gridx x = mkP2 x (ptoy minp) ~~ mkP2 x (ptoy maxp)
                  # lw 1
                  # lc lightgrey
        pitch n = text n # fontSizeL 0.6
        innerNodes = atPoints (zipWith mkP2 xs ys) (map node vs)

graphDiag g = frame 1 $ graphDiag' g def

sumGraphDiag g m = frame 1 $ graphDiag' g (def { goEdgeOpts = edgeOpts })
  where vals = sqrt . fromIntegral <$> m
        factor = 3 / (sum vals / fromIntegral (M.size vals))
        edgeOpts e opts = opts
                          & shaftStyle %~ lw w
                          & headLength .~ (5 * w)
          where w = M.findWithDefault 1 e vals * factor

myRender :: Diagram B -> IO ()
myRender = myRender' "diagram" 1500
-- myRender diag = renderCanvas 3000 (dims2D 1000 400) diag

myRender' :: FilePath -> Double -> Diagram B -> IO ()
myRender' name w diag = renderSVG ("/tmp/" <> name <> ".svg") (dims2D w h) diag
  where h = height diag * (w / width diag)
