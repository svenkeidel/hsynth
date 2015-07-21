module Sound.AllPassFilter(allPassFilter,allPassFilterTime) where

import           Sound.Types
import           Sound.Delay

type Alpha = Double
type Delay = Audio -> Audio
type DelayLength = Int

allPassFilter :: DelayLength -> Alpha -> Audio -> Audio
allPassFilter delayLength = allPassFilterWithDelay (delay delayLength)
{-# INLINE allPassFilter #-}

allPassFilterTime :: Duration -> Rate -> Alpha -> Audio -> Audio
allPassFilterTime dur rate = allPassFilterWithDelay (delayTime dur rate)
{-# INLINE allPassFilterTime #-}

allPassFilterWithDelay :: Delay -> Alpha -> Audio -> Audio
allPassFilterWithDelay delayFun alpha x =
  let out = fmap (negate alpha *) x + delayFun (fmap (alpha *) out + x)
  in out
{-# INLINE allPassFilterWithDelay #-}
