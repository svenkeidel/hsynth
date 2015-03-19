module Music.Chord where

import           Music.Pitch
import           Music.Interval (Interval,(<+),min3,maj3,per5)

type Chord = [Pitch]

type Root = Pitch

triad :: Interval -> Interval -> Root -> Chord
triad i1 i2 p = [ p, p <+ i1, p <+ i2 ]
{-# INLINE triad #-}

major :: Root -> Chord
major = triad maj3 per5
{-# INLINE major #-}

minor :: Root -> Chord
minor = triad min3 per5
{-# INLINE minor #-}

diminished :: Pitch -> Chord
diminished p = [p+0,p+3,p+6]

augmented :: Pitch -> Chord
augmented p = [p+0,p+4,p+8]
