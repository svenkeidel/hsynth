module Sound.Amplifier where

import           Prelude ()
import           Language.Frontend

amp :: Double -> Signal Double Double
amp c = arr (\x -> double c * x)
