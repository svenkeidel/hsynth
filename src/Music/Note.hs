module Music.Note where

import           Data.Ratio
import           Music.Pitch
import           Music.Types

data Note = Note Pitch Duration

note :: PitchClass -> Octave -> Duration -> Note
note pc octave = Note (pitch pc octave)
{-# INLINE note #-}

noteDuration :: Duration -> PitchClass -> Octave -> Note
noteDuration duration pc octave = note pc octave duration
{-# INLINE noteDuration #-}

double, breve :: PitchClass -> Octave -> Note
double = noteDuration 2
breve = double
{-# INLINE double #-}
{-# INLINE breve #-}

whole, semibreve :: PitchClass -> Octave -> Note
whole = noteDuration 1
semibreve = whole
{-# INLINE whole #-}
{-# INLINE semibreve #-}

half, minim :: PitchClass -> Octave -> Note
half = noteDuration (1%2)
minim = half
{-# INLINE half #-}
{-# INLINE minim #-}

quater, crotchet :: PitchClass -> Octave -> Note
quater = noteDuration (1%4)
crotchet = quater
{-# INLINE quater #-}
{-# INLINE crotchet #-}

eighth, quaver :: PitchClass -> Octave -> Note
eighth = noteDuration (1%8)
quaver = eighth
{-# INLINE eighth #-}
{-# INLINE quaver #-}

sixteenth, semiquaver :: PitchClass -> Octave -> Note
sixteenth = noteDuration (1%16)
semiquaver = sixteenth
{-# INLINE sixteenth #-}
{-# INLINE semiquaver #-}
