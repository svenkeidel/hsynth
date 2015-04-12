module Music.ConcertPitch where

import Music.Pitch
import Sound.Types

data ConcertPitch = ConcertPitch 
  { pitch     :: Pitch
  , frequency :: Frequency
  }
