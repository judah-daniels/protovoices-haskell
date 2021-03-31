module Display where

import           Common

import           Diagrams.Prelude        hiding ( (^-^)
                                                , (^+^)
                                                )
import qualified Diagrams.TwoD.Text
import           Diagrams.Backend.SVG          as SVG
import           Data.Typeable                  ( Typeable )
import           Data.Colour

import qualified Data.Set                      as S

import           Control.Monad                  ( mzero )
import           Control.Monad.Trans            ( lift )
import qualified Control.Monad.State           as ST
import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.List                      ( sortOn )
import           Data.Foldable                  ( foldl' )

type PVDiagram b = QDiagram b V2 Double Any

-- derivation graphs
-- =================

type DerivSlice a = (Int, Int, StartStop a)
type DerivTrans a e = (DerivSlice a, e, DerivSlice a)

data DerivationGraph a e = DGraph
  { dgNextId :: Int
  , dgSlices :: S.Set (DerivSlice a)
  , dgTransitions :: S.Set (DerivTrans a e)
  , dgHoriEdges :: S.Set (DerivSlice a, DerivSlice a)
  , dgSurface :: [DerivTrans a e]
  , dgFoot :: [DerivTrans a e]
  , dgRoot :: DerivTrans a e
  }
  deriving (Eq, Ord, Show)

type DerivationOp a e = ST.StateT (DerivationGraph a e) (Either String)

popSurface :: DerivationOp a e (DerivTrans a e)
popSurface = do
  graph <- ST.get
  case dgSurface graph of
    []     -> mzero
    t : ts -> do
      ST.put graph { dgSurface = ts }
      pure t

pushOpen :: (Ord a, Ord e) => [DerivTrans a e] -> DerivationOp a e ()
pushOpen newts = do
  graph <- ST.get
  let trans' = S.fromList newts <> dgTransitions graph
      surf'  = newts <> dgSurface graph
  ST.put $ graph { dgTransitions = trans', dgSurface = surf' }

pushClosed :: (Ord a, Ord e) => DerivTrans a e -> DerivationOp a e ()
pushClosed newt = do
  graph <- ST.get
  let trans' = S.insert newt $ dgTransitions graph
      foot'  = newt : dgFoot graph
  ST.put $ graph { dgTransitions = trans', dgFoot = foot' }

addSlice :: Ord a => a -> Int -> DerivationOp a e (DerivSlice a)
addSlice slice depth = do
  graph <- ST.get
  let i        = dgNextId graph
      newSlice = (depth, i, Inner slice)
      slices'  = S.insert newSlice $ dgSlices graph
  ST.put $ graph { dgNextId = i + 1, dgSlices = slices' }
  pure newSlice

addHoriEdge :: Ord a => (DerivSlice a, DerivSlice a) -> DerivationOp a e ()
addHoriEdge edge = do
  graph <- ST.get
  let horis' = S.insert edge $ dgHoriEdges graph
  ST.put $ graph { dgHoriEdges = horis' }

data DerivationPlayer s f h a e = DerivationPlayer
  { dpRoot :: e
  , dpSplit :: s -> e -> Either String (e, a, e)
  , dpFreeze :: f -> e -> Either String e
  , dpHorizontalize :: h -> e -> a -> e -> Either String (e, a, e, a, e)
  }

replayDerivation
  :: (Ord a, Ord e)
  => [Common.Leftmost s f h]
  -> DerivationPlayer s f h a e
  -> Either String (DerivationGraph a e)
replayDerivation deriv player = ST.execStateT (mapM_ applyRule deriv) init
 where
  start = (0, 0, (:⋊))
  end   = (0, 1, (:⋉))
  top   = (start, dpRoot player, end)
  init =
    DGraph 2 (S.fromList [start, end]) (S.singleton top) S.empty [top] [] top
  applyRule (LMSplitLeft s) = do
    (pl, pt, pr) <- popSurface
    (cl, cm, cr) <- lift $ dpSplit player s pt
    let (depthl, _, _) = pl
        (depthr, _, _) = pr
    sm <- addSlice cm $ max depthl depthr + 1
    pushOpen [(pl, cl, sm), (sm, cr, pr)]
  applyRule (LMSplitRight s) = do
    l            <- popSurface
    (pl, pt, pr) <- popSurface
    (cl, cm, cr) <- lift $ dpSplit player s pt
    let (depthl, _, _) = pl
        (depthr, _, _) = pr
    sm <- addSlice cm $ max depthl depthr + 1
    pushOpen [l, (pl, cl, sm), (sm, cr, pr)]
  applyRule (LMFreeze f) = do
    (pl, pt, pr) <- popSurface
    t            <- lift $ dpFreeze player f pt
    pushClosed (pl, t, pr)
  applyRule (LMHorizontalize h) = do
    (lpl, lpt, pm ) <- popSurface
    (_  , rpt, rpr) <- popSurface
    let (depthl, _, _      ) = lpl
        (depthm, _, pmLabel) = pm
        (depthr, _, _      ) = rpr
        depth'               = max depthl (max depthm depthr) + 1
    pmInner           <- lift $ getInnerE pmLabel
    (l, lc, m, rc, r) <- lift $ dpHorizontalize player h lpt pmInner rpt
    ls                <- addSlice lc depth'
    rs                <- addSlice rc depth'
    addHoriEdge (pm, ls)
    addHoriEdge (pm, rs)
    pushOpen [(lpl, l, ls), (ls, m, rs), (rs, r, rpr)]

derivationPlayerUnit :: DerivationPlayer s f h () ()
derivationPlayerUnit = DerivationPlayer root split freeze hori
 where
  root = ()
  split _ _ = Right ((), (), ())
  freeze _ _ = Right ()
  hori _ _ _ _ = Right ((), (), (), (), ())

-- plotting derivation graphs
-- ==========================

tikzDerivationGraph
  :: (a -> String) -> (b -> String) -> DerivationGraph a b -> String
tikzDerivationGraph showS showT (DGraph _ slices trans horis _ foot root) =
  L.intercalate
    "\n"
    (  (showNode <$> tikzNodes)
    <> (showTrans <$> S.toList trans)
    <> (showHori <$> S.toList horis)
    )
 where
  -- printing nodes and edges
  showSlice (:⋊)      = "$\\rtimes$"
  showSlice (:⋉)      = "$\\ltimes$"
  showSlice (Inner s) = showS s
  showNode (x, y, id, c) =
    "\\node[slice] (slice"
      <> show id
      <> ") at ("
      <> show x
      <> ","
      <> show (-y)
      <> ") {"
      <> showSlice c
      <> "};"
  showTrans (nl, c, nr) =
    "\\draw[transition] (slice"
      <> show (getID nl)
      <> ") -- (slice"
      <> show (getID nr)
      <> ");"
  showHori (p, c) =
    "\\draw[hori] (slice"
      <> show (getID p)
      <> ") -- (slice"
      <> show (getID c)
      <> ");"
  -- helpers
  getID (_, i, _) = i
  getDepth (d, _, _) = d
  leftNode (n, _, _) = n
  rightNode (_, _, n) = n
  -- computing node locations
  nodeChildren =
    M.fromListWith (++) $ bimap getID ((: []) . getID) <$> S.toList horis
  foot'        = reverse foot
  surfaceNodes = fmap getID $ leftNode (head foot') : fmap rightNode foot'
  allNodes     = getID <$> sortOn getDepth (S.toList slices)
  -- compute x locations
  xloc         = foldl' findX xlocInit allNodes
   where
    xlocInit = M.fromList $ zip surfaceNodes [0.0 :: Double ..]
    mean xs = sum xs / fromIntegral (length xs)
    -- findX :: M.Map Int Double -> Int -> M.Map Int Double
    findX locs id = case M.lookup id locs of
      Just x -> locs
      Nothing ->
        let children = nodeChildren M.! id
            childxs  = (locs M.!) <$> children
            x        = mean childxs
        in  M.insert id x locs
  tikzNodes = mkNode <$> S.toList slices
    where mkNode (depth, id, content) = (xloc M.! id, depth, id, content)

showTex x = concatMap escapeTex $ show x
 where
  escapeTex '♭' = "$\\flat$"
  escapeTex '♯' = "$\\sharp$"
  escapeTex '{' = "\\{"
  escapeTex '}' = "\\}"
  escapeTex c   = [c]
