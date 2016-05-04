module Sound.Envelope where

import           Prelude (undefined)
import           Language.Frontend
import           Sound.Types

lineSeg :: [(Amplitude,Duration)] -> Amplitude -> Rate -> Signal () Double
lineSeg = undefined

lineSeg' :: Amplitude -> Duration -> Amplitude -> Rate -> Signal () Double
lineSeg' a1 dur a2 rate =
  let dr = dur * fromIntegral rate
      n  = truncate dr
      delta = (a2 - a1) / dr
  in undefined
