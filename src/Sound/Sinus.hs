module Sound.Sinus where

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
      sine = 0 <:> i <:> S.zipWith (\b a -> c * a - b) sine (S.tail sine)
  in sine
{-# INLINE sinA #-}

