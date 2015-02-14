module Sound.Types where

import Data.Stream (Stream)

type Rate = Int
type Frequency = Double
type Duration = Double
type Audio = Stream Double
type Amplitude = Double
type Envelope = Stream Double
