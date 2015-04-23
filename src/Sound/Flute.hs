module Sound.Flute where

import qualified Data.Stream as S

import           Sound.Types
import           Sound.LineSeg
import           Sound.Random
import           Sound.Sinus
import           Sound.Delay
import           Sound.Lowpass
import           Sound.LowFrequencyOscillator

import           System.Random (RandomGen)

type Pressure = Double 
type Breath = Double 

flute :: RandomGen g => Duration -> Amplitude -> Frequency -> Pressure -> Breath -> g -> Rate -> Audio
flute dur amp freq pressure breath g rate =
  let env1   = lineSegLFO [(0,0.06),(1.1 * pressure,0.2),(pressure,dur - 0.16),(pressure,0.02)] 0 rate
      env2   = lineSegLFO [(0,0.01),(1,dur-0.02),(pressure,0.01)] 0 rate
      envib  = lineSegLFO [(0,0.5),(0,0.5),(1,dur - 1)] 1 rate
      flow   = random g * env1
      vibr   = sinA 5 rate * vibrato * envib
      bore   = delayTime (1 / freq) rate
      emb    = delayTime (1 / freq / 2) rate
      flute' = bore out
      x      = emb (S.repeat breath * flow + env1 + vibr + flute' * feedback1)
      out    = lowpass 0.27 (x - x**3 + flute' * feedback2)
  in out * S.repeat amp * env2

  where
    lineSegLFO amps hold = lfo 100 (lineSeg amps hold)
    vibrato = 0.03
    feedback1 = 0.4
    feedback2 = 0.4
