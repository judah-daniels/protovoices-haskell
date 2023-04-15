module Harmony.ChordLabel
  ( ChordType (..)
  , ChordLabel (..)
  , mkLbl
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
  deriving (Eq, Enum, Bounded, Ord)

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
      _ -> []

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

data ChordLabel = ChordLabel
  { chordType :: ChordType
  , rootNote :: SPC
  }
  deriving (Generic, Eq, Ord)

instance Show ChordLabel where
  show (ChordLabel lbl root) = showNotation root <> show lbl

mkLbl rootInt chordType = ChordLabel chordType (spc (rootInt - 14))
