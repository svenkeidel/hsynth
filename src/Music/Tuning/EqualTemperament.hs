-- |Divides the octave into equal sized intervals. The number of interval may
-- vary. The most common tuning in western music these days is 12-tone equal
-- temperament, i.e. the octave gets devided into 12 intervals.
module Music.Tuning.EqualTemperament where

import           Music.Pitch
import           Music.ConcertPitch (ConcertPitch)
import qualified Music.ConcertPitch as CP
import           Sound.Types

-- |Number of tones in an octave.
type Tones = Int
data EqualTemperament =
  EqualTemperament
  { order :: Pitch -> Ordinal
  , tones :: Tones
  }

equalTemperament :: EqualTemperament -> ConcertPitch -> Pitch -> Frequency
equalTemperament et concertPitch p =
  freqC0 * 2 ** (n / t)
  where
    freqC0 = (CP.frequency concertPitch) / (2 ** (fromIntegral (order et (CP.pitch concertPitch)) / t))
    n = fromIntegral (order et p)
    t = fromIntegral (tones et)
{-# INLINE equalTemperament #-}

twelveTET :: EqualTemperament
twelveTET = EqualTemperament
  { order = \(Pitch pc octave) ->
        let pcOrd = horizontal pc
        in octave * 12 + pcOrd - 57
  , tones = 12
  }
{-# INLINE twelveTET #-}
