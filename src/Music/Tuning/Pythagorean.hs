module Music.Tuning.Pythagorean where

import Sound.Types (Frequency)
import Music.Pitch (PitchClass)
import Music.JustInterval

type Generator = Interval
type Ordinal = Int

pythagorean :: Generator -> Ordinal -> Frequency
pythagorean gen ord =
  let i = ord
      i' = fromIntegral i :: Double
      k = if i < 0 then truncate ((-i'+2) / 2) else negate (truncate (i'/2)) :: Int
      g = ratio gen
  in fromRational (g^i * 2^k)



pitchClassToInt :: PitchClass -> Int
pitchClassToInt = undefined
  
