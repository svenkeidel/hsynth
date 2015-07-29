module Music.ConcertPitch where

import           Music.Pitch (Pitch)
import qualified Music.Pitch as P

import           Sound.Types (Frequency)

data ConcertPitch = ConcertPitch
  { pitch     :: Pitch
  , frequency :: Frequency
  }

a440 :: ConcertPitch
a440 = ConcertPitch (P.pitch P.A 4) 440
