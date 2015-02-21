module Sound.Lowpass where

import           Data.Stream ((<:>))
import qualified Data.Stream as S

import           Sound.Types
import           Sound.BiQuadraticFilter

lowpass :: Double -> Audio -> Audio
lowpass c = S.scan (\o i -> o + c * (i - o)) 0
{-# INLINE lowpass #-}

lowpassBi :: Resonance -> Frequency -> Rate -> Audio -> Audio
lowpassBi q freq rate =
  let k = tan (pi * freq / fromIntegral rate)
      a0 = k**2 / (k**2 + k/q + 1)
      a1 = 2 * a0
      a2 = a0
      b1 = 2 * (k**2 - 1) / (k**2 + k/q + 1)
      b2 = (k**2 - k/q + 1) / (k ** 2 + k/q + 1)
  in biquad a0 a1 a2 b1 b2
{-# INLINE lowpassBi #-}
