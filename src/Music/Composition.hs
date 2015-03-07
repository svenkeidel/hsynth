module Music.Composition where

import Music.Note
import Music.Rest(IsRest(..))

data Atom = Notes [Note] | Rest Rational
type Composition = [Atom]

instance IsRest Atom where
  mkRest = Rest

instance IsNote Atom where
  mkNote p d = Notes [mkNote p d]

notes :: [Note] -> Atom
notes = Notes
