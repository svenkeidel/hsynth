module Sound.Saw where

import           Prelude ()
import           Language.Frontend
import           Sound.Types

saw :: Frequency -> Rate -> Signal () Double
saw freq rate =
    let dt = 2 * freq / fromIntegral rate
    in unfold (\x -> let x' = x + double dt in (x,If (x' <= 1) x' (x' - 2))) (0 :: Double)
