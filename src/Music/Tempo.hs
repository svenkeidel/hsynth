module Music.Tempo where

import Sound.Types (Time)

data BPM = BPM Int Length
type Length = Rational

beatToTime :: BPM -> Rational -> Time
beatToTime (BPM bpm tick) beat =
  fromIntegral bpm * fromRational beat
    / (fromRational tick * 60)
