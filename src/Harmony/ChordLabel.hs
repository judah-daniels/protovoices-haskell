module Harmony.ChordLabel
  ( ChordType (..)
  , ChordLabel (..)
  , mkLbl
  , enharmonicLabels
  , chordToneProfile
  , allChordLabels
  )
where

import GHC.Generics
import Musicology.Pitch.Class
import Musicology.Pitch.Spelled

chordTypes = ["M", "m", "Mm7", "o", "o7", "mm7", "%7", "MM7", "+", "Ger", "It", "Fr", "mM7", "+7"]

-- | DCML Chord Types
data ChordType
  = Major
  | Minor
  | DominantSeventh
  | Diminished
  | FullDiminished
  | MinorSeventh
  | HalfDiminished
  | MajorSeventh
  | Augmented
  | GermanSixth
  | ItalianSixth
  | FrenchSixth
  | MinorMajorSeventh
  | AugmentedSeventh
  | NoChord
  deriving (Eq, Enum, Bounded, Ord)

chordToneProfile :: ChordType -> [Int]
chordToneProfile chordType = case chordType of
  Major -> [0, 1, 4]
  Minor -> [0, 1, -3]
  DominantSeventh -> [0, 1, 4, -2]
  Diminished -> [0, -3, -6]
  FullDiminished -> [0, -3, -6, -9]
  MinorSeventh -> [0, 1, -2, -3]
  HalfDiminished -> [0, -3, -6, -2]
  MajorSeventh -> [0, 1, 4, 5]
  Augmented -> [0, 4, 8]
  GermanSixth -> [0, -10, -6, -9]
  ItalianSixth -> [0, -10, -6]
  FrenchSixth -> [0, 4, -6, -2]
  MinorMajorSeventh -> [0, 1, -3, 5]
  AugmentedSeventh -> [0, 4, 8, -2]
  NoChord -> [-14 .. 14]

instance Read ChordType where
  readsPrec _ str =
    case str of
      "M" -> [(Major, "")]
      "m" -> [(Minor, "")]
      "Mm7" -> [(DominantSeventh, "")]
      "o" -> [(Diminished, "")]
      "o7" -> [(FullDiminished, "")]
      "mm7" -> [(MinorSeventh, "")]
      "%7" -> [(HalfDiminished, "")]
      "MM7" -> [(MajorSeventh, "")]
      "+" -> [(Augmented, "")]
      "Ger" -> [(GermanSixth, "")]
      "It" -> [(ItalianSixth, "")]
      "Fr" -> [(FrenchSixth, "")]
      "mM7" -> [(MinorMajorSeventh, "")]
      "+7" -> [(AugmentedSeventh, "")]
      _ -> [(NoChord, "")]

instance Show ChordType where
  show chordType =
    case chordType of
      Major -> "M"
      Minor -> "m"
      DominantSeventh -> "Mm7"
      Diminished -> "o"
      FullDiminished -> "o7"
      MinorSeventh -> "mm7"
      HalfDiminished -> "%7"
      MajorSeventh -> "MM7"
      Augmented -> "+"
      GermanSixth -> "Ger"
      ItalianSixth -> "It"
      FrenchSixth -> "Fr"
      MinorMajorSeventh -> "mM7"
      AugmentedSeventh -> "+7"
      NoChord -> "NC"

data ChordLabel = ChordLabel
  { chordType :: ChordType
  , rootNote :: SPC
  }
  deriving (Generic, Eq, Ord)

instance Show ChordLabel where
  show (ChordLabel lbl root) = showNotation root <> show lbl

enharmonicLabels :: ChordLabel -> [ChordLabel]
enharmonicLabels lbl = [lbl, shiftProfileLeft lbl, shiftProfileRight lbl, shiftProfileLeft . shiftProfileLeft $ lbl, shiftProfileRight . shiftProfileRight $ lbl]
 where
  shiftProfileLeft lbl@(ChordLabel chordType rootNote) =
    let rootNote' = transpose (sic 12) rootNote
     in ChordLabel chordType rootNote'

  shiftProfileRight lbl@(ChordLabel chordType rootNote) =
    let rootNote' = transpose (sic (-12)) rootNote
     in ChordLabel chordType rootNote'

allChordLabels :: [ChordLabel]
allChordLabels = do
  chordType <- [minBound .. maxBound]
  rootNote <- map spc [-14 .. 14]
  pure $ ChordLabel chordType rootNote

mkLbl rootInt chordType = ChordLabel chordType (spc (rootInt - 14))
