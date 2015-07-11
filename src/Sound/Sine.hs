module Sound.Sine where

import           Control.Category

import           Data.Stream ((<:>))
import qualified Data.Stream as S

import           Sound.Types
import           Sound.Integral

oscSine :: Frequency -> Rate -> Audio -> Audio
oscSine f0 rate =
      fmap (\freq -> 2 * pi * f0 * 2**freq)
  >>> integral rate
  >>> fmap sin
{-# INLINE oscSine #-}

sinA :: Frequency -> Rate -> Audio
sinA freq rate=
  let omh  = 2 * pi * freq / fromIntegral rate
      i    = sin omh
      c    = 2 * cos omh
  in S.unfold (\(y1,y2) -> let y = c * y1 - y2 in (y2,(y,y1))) (i,0)
{-# INLINE sinA #-}

