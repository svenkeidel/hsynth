module Music.Composition where

import Music.Note
import Music.Rest

data Atom = Notes [Note] | Rest Rest
type Composition = [Atom]
