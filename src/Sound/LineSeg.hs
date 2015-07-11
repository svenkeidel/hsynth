module Sound.LineSeg(lineSeg) where

import qualified Data.Stream as S

import           Sound.Types

lineSeg :: [(Amplitude,Duration)] -> Amplitude -> Rate -> Envelope
lineSeg ((a1,d1):(a2,d2):r) hold rate =
  lineSeg' a1 d1 a2 rate
    (lineSeg ((a2,d2):r) hold rate)
lineSeg [(a1,d1)] hold rate =
  lineSeg' a1 d1 hold rate
      (lineSeg [] hold rate)
lineSeg [] hold _ = S.repeat hold

lineSeg' :: Amplitude -> Duration -> Amplitude -> Rate -> Envelope -> Envelope
lineSeg' a1 dur a2 rate rest =
  let dr    = dur * fromIntegral rate
      n     = truncate dr
      delta = (a2 - a1) / dr
  in S.switch n
        (S.iterate (+delta) a1)
        rest
{-# INLINE lineSeg' #-}
