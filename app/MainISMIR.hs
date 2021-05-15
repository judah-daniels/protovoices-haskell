{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Parser
import           PVGrammar
import           PVGrammar.Parse
import           PVGrammar.Generate
import           Common
import           Display

import           Musicology.Core
import           Musicology.Pitch.Spelled      as MT
import           Musicology.Core.Slicing
import           Musicology.MusicXML

import           Data.Maybe                     ( catMaybes )

-- better do syntax
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )


deriv321sus :: [PVLeftMost (Pitch MT.SIC)]
deriv321sus = buildDerivation $ do
  split $ mkSplit $ do
    splitT (:⋊) (:⋉) (c' nat) RootNote False False
    splitT (:⋊) (:⋉) (e' nat) RootNote False False
  hori $ mkHori $ do
    horiNote (c' nat) ToBoth     True
    horiNote (e' nat) (ToLeft 1) False
    addPassing (e' nat) (c' nat)
  freeze FreezeOp
  split $ mkSplit $ do
    splitT (Inner $ c' nat) (Inner $ c' nat) (b' nat) FullNeighbor True False
    splitNT (e' nat) (c' nat) (d' nat) PassingMid True False
  split $ mkSplit $ do
    splitT (Inner $ e' nat)
           (Inner $ d' nat)
           (d' nat)
           LeftRepeatOfRight
           False
           True
    splitT (Inner $ c' nat)
           (Inner $ b' nat)
           (c' nat)
           RightRepeatOfLeft
           True
           False
  freeze FreezeOp
  freeze FreezeOp
  freeze FreezeOp
  freeze FreezeOp

path321sus =
  Path [e' nat, c' nat] [(Inner $ c' nat, Inner $ c' nat)]
    $ Path [d' nat, c' nat] [(Inner $ d' nat, Inner $ d' nat)]
    $ Path [d' nat, b' nat] []
    $ PathEnd [c' nat]

plotDeriv fn deriv = do
  case replayDerivation derivationPlayerPV deriv of
    (Left  err) -> putStrLn err
    (Right g  ) -> viewGraph fn g

slicesFromFile :: FilePath -> IO [[(SPitch, RightTied)]]
slicesFromFile file = do
  txt <- readFile file
  let notes  = asNote <$> xmlNotesHeard txt
      slices = slicePiece tiedSlicer notes
  return $ mkSlice <$> filter (not . null) slices
 where
  mkSlice notes = mkNote <$> notes
  mkNote (note, tie) = (pitch note, rightTie tie)

slicesToPath
  :: (Interval i, Ord (ICOf i), Eq i)
  => [[(Pitch i, RightTied)]]
  -> Path [Pitch (ICOf i)] [Edge (Pitch (ICOf i))]
slicesToPath = go
 where
  mkSlice = fmap (pc . fst)
  mkEdges notes = catMaybes $ mkEdge <$> notes
   where
    mkEdge (p, Ends ) = Nothing
    mkEdge (p, Holds) = let p' = pc p in Just (Inner p', Inner p')
  go []             = error "cannot construct path from empty list"
  go [notes       ] = PathEnd (mkSlice notes)
  go (notes : rest) = Path (mkSlice notes) (mkEdges notes) $ go rest

main :: IO ()
main = do
  -- plotDeriv "ismir-out/321sus.tex" deriv321sus
  count321sus <- parseSilent pvCount path321sus
  bachSlices  <- slicesFromFile "testdata/allemande.musicxml"
  bachCount   <- parseSize pvCount $ slicesToPath $ take 9 bachSlices
  putStrLn $ "\nnumber of derivations (321sus): " <> show count321sus
  putStrLn $ "number of derivations (bach): " <> show bachCount
