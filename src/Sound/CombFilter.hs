module Sound.CombFilter (combFilter,combFilterTime) where

import           Sound.Types
import           Sound.Delay

type Alpha = Double
type DelayLength = Int
type Delay = Audio -> Audio

combFilter :: DelayLength -> Alpha -> Audio -> Audio
combFilter delayLength = combFilterWithDelay (delay delayLength)
{-# INLINE combFilter #-}

combFilterTime :: Duration -> Rate -> Alpha -> Audio -> Audio
combFilterTime dur rate = combFilterWithDelay (delayTime dur rate)
{-# INLINE combFilterTime #-}

combFilterWithDelay :: Delay -> Alpha -> Audio -> Audio
combFilterWithDelay delayFun alpha x =
  x + fmap (alpha*) (delayFun x)
{-# INLINE combFilterWithDelay #-}
