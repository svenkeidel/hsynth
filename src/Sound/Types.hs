module Sound.Types where

import Data.Stream (Stream)

type Rate = Int
type Frequency = Double
type Duration = Double
type Time = Double
type Audio = Stream Double
type AudioFun a = ((),a) -> (Double,a)
type AudioProcess a = (a,AudioFun a)
type Amplitude = Double
type Envelope = Stream Double
type Resonance = Double
