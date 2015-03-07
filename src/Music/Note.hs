module Music.Note where

import           Data.Ratio
import           Music.Pitch
import           Music.Types

data Note = Note Pitch Duration

class IsNote n where
  mkNote :: Pitch -> Duration -> n

instance IsNote Note where
  mkNote = Note

note :: IsNote n => PitchClass -> Octave -> Duration -> n
note pc octave = mkNote (pitch pc octave)
{-# INLINE note #-}

noteDuration :: IsNote n => Duration -> PitchClass -> Octave -> n
noteDuration duration pc octave = note pc octave duration
{-# INLINE noteDuration #-}

double, breve :: IsNote n => PitchClass -> Octave -> n
double = noteDuration 2
breve = double
{-# INLINE double #-}
{-# INLINE breve #-}

whole, semibreve :: IsNote n => PitchClass -> Octave -> n
whole = noteDuration 1
semibreve = whole
{-# INLINE whole #-}
{-# INLINE semibreve #-}

half, minim :: IsNote n => PitchClass -> Octave -> n
half = noteDuration (1%2)
minim = half
{-# INLINE half #-}
{-# INLINE minim #-}

quater, crotchet :: IsNote n => PitchClass -> Octave -> n
quater = noteDuration (1%4)
crotchet = quater
{-# INLINE quater #-}
{-# INLINE crotchet #-}

eighth, quaver :: IsNote n => PitchClass -> Octave -> n
eighth = noteDuration (1%8)
quaver = eighth
{-# INLINE eighth #-}
{-# INLINE quaver #-}

sixteenth, semiquaver :: IsNote n => PitchClass -> Octave -> n
sixteenth = noteDuration (1%16)
semiquaver = sixteenth
{-# INLINE sixteenth #-}
{-# INLINE semiquaver #-}
