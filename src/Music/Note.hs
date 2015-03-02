module Music.Note
  ( Note
  , note
  , double, breve
  , whole, semibreve
  , half, minim
  , quater, crotchet
  , eighth, quaver
  , sixteenth, semiquaver
  ) where

import           Data.Ratio
import           Music.Pitch
import           Music.Types

data Note = Note Pitch Duration

note :: PitchClass -> Octave -> Duration -> Note
note pc octave = Note (pitch pc octave)
{-# INLINE note #-}

noteDuration :: Duration -> PitchClass -> Octave -> Note
noteDuration duration pc octave = note pc octave duration

double, breve :: PitchClass -> Octave -> Note
double = noteDuration 2
breve = double

whole, semibreve :: PitchClass -> Octave -> Note
whole = noteDuration 1
semibreve = whole

half, minim :: PitchClass -> Octave -> Note
half = noteDuration (1%2)
minim = half

quater, crotchet :: PitchClass -> Octave -> Note
quater = noteDuration (1%4)
crotchet = quater

eighth, quaver :: PitchClass -> Octave -> Note
eighth = noteDuration (1%8)
quaver = eighth

sixteenth, semiquaver :: PitchClass -> Octave -> Note
sixteenth = noteDuration (1%16)
semiquaver = sixteenth
