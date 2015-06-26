module Sound.Compressor where

import Sound.Types

type Threshold = Double
type Ratio = Double
type KneeWidth = Double
type Gain = Double
type GainComputer = Gain -> Gain

hardKnee :: Threshold -> Ratio -> GainComputer
hardKnee t r x
  | x <= t    = x
  | otherwise = t + (x - t) / r

softKnee :: Threshold -> Ratio -> KneeWidth -> GainComputer
softKnee t r w x
  | 2 * (x - t) < -w     = x
  | 2 * abs (x - t) <= w = x + (1/r - 1) * (x - t + w / 2) ^^ 2 / (2*w)
  | otherwise            = t + (x - t) / r

feadForward :: GainComputer -> Amplitude -> Amplitude
feadForward gc x = x + gc x - x
