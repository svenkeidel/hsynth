module Sound.Discretization where

import Data.Word

discretize8 :: Double -> Word8
discretize8 x = round $ (x + 1) / 2 * fromIntegral (maxBound :: Word8) 
