-- |Divides the octave into equal sized intervals. The number of interval may
-- vary. The most common tuning in western music these days is 12-tone equal
-- temperament, i.e. the octave gets devided into 12 intervals.
module Music.Tuning.EqualTemperament where

import Music.Pitch
import Sound.Types

-- |Number of tones in an octave.
type Tones = Int
data EqualTemperament =
  EqualTemperament
  { order         :: Pitch -> Ordinal
  , tones         :: Tones
  , baseFrequency :: Frequency
  }

equalTemperament :: EqualTemperament -> Pitch -> Frequency
equalTemperament et p = f0 * 2 ** (n / t)
  where
    f0 = baseFrequency et
    n = fromIntegral (order et p)
    t = fromIntegral (tones et)
{-# INLINE equalTemperament #-}

twelveTET :: EqualTemperament
twelveTET = EqualTemperament
  { order = \(Pitch pc octave) ->
        let pcOrd = case pc of
              Cff -> -2; Cf -> -1; C ->  0; Cs ->  1; Css ->  2
              Dff ->  0; Df ->  1; D ->  2; Ds ->  3; Dss ->  4
              Eff ->  2; Ef ->  3; E ->  4; Es ->  5; Ess ->  6
              Fff ->  3; Ff ->  4; F ->  5; Fs ->  6; Fss ->  7
              Gff ->  5; Gf ->  6; G ->  7; Gs ->  8; Gss ->  9
              Aff ->  7; Af ->  8; A ->  9; As -> 10; Ass -> 11
              Bff ->  9; Bf -> 10; B -> 11; Bs -> 12; Bss -> 13 
        in octave * 12 + pcOrd - 57
  , tones = 12
  , baseFrequency = 440
  }
{-# INLINE twelveTET #-}
