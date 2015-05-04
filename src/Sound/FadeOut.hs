module Sound.FadeOut where

import qualified Data.Stream as S

import           Sound.LineSeg
import           Sound.LowFrequencyOscillator
import           Sound.Types

fadeOut :: Duration -> Rate -> Audio -> [Double]
fadeOut dur rate audio = 
  S.take (truncate (dur * fromIntegral rate))
    $ audio * lfo 100 (lineSeg [(1,dur)] 0) rate
