module Sound.BiQuadraticFilter where

import qualified Data.Stream as S

import           Sound.Types
import           Sound.Delay

type A0 = Double
type A1 = Double
type A2 = Double
type B1 = Double
type B2 = Double

-- Transposed direct form II
-- source: http://www.earlevel.com/main/2003/02/28/biquads/
biquad :: A0 -> A1 -> A2 -> B1 -> B2 -> Audio -> Audio
{-biquad a0 a1 a2 b1 b2 inp =-}
  {-let out = S.repeat a0 * inp + z1-}
      {-z1 = delay1 0 (S.repeat a1 * inp + S.repeat b1 * out + z2)-}
      {-z2 = delay1 0 (S.repeat a2 * inp + S.repeat b2 * out)-}
  {-in out-}
biquad a0 a1 a2 b1 b2 =
  let go (_,z1,z2) i =
        let o   = i * a0 + z1
            z1' = i * a1 - o * b1 + z2
            z2' = i * a2 - o * b2
        in (o,z1',z2')
  in fmap (\(o,_,_) -> o) . S.scan go (0,0,0)
{-# INLINE biquad #-}
