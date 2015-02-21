module Sound.BaseDrum where

import           Sound.Types
import           Sound.LineSeg
import           Sound.Sinus

kickDrum :: Frequency -> Rate -> Audio
kickDrum freq rate =
  let pitchEnv = lineSeg [(0,5)] (-5) rate
      ambEnv   = lineSeg [(1,5)] 0 rate
  in ambEnv * oscSine freq rate pitchEnv
