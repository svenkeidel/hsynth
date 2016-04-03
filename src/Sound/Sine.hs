{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Sound.Sine where

import           Prelude ()
import           Language.Frontend
import           Sound.Types

oscSine :: Frequency -> Rate -> Signal Double Double
oscSine f0 rate =
      arr (\freq -> double (2 * pi * f0) * double 2 ** freq)
  >>> integral rate
  >>> arr (sin :: Expr Double -> Expr Double)

sinA :: Frequency -> Rate -> Signal () Double
sinA freq rate =
  let omh  = 2 * pi * freq / fromIntegral rate
      i    = sin omh :: Double
      c    = 2 * cos omh :: Double
  in unfold (\(y1,y2) -> let y = double c * y1 - y2 in (y2,(y,y1)))
            (i,0)

add :: Double -> Signal Double Double
add c = arr (\x -> double c + x)
