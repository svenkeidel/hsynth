module Sound.LowFrequencyOscillator where

import           Sound.Types
import qualified Data.Stream as S

type Factor = Int

lfo :: Factor -> (Rate -> Audio) -> Rate -> Audio
lfo factor f rate =
  upsample factor $ f (rate `div` factor)
{-# INLINE lfo #-}

upsample :: Factor -> Audio -> Audio
upsample n (S.Cons a as) = S.switch n (S.repeat a) (upsample n as)
