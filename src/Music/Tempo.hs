module Music.Tempo where

import Sound.Types (Time)

data BPM = BPM Int Beat
type Beat = Rational

beatToTime :: BPM -> Beat -> Time
beatToTime (BPM bpm tick) beat =
  fromIntegral bpm * fromRational beat
    / (fromRational tick * 60)
