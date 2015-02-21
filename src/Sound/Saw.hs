module Sound.Saw where

import           Sound.Time
import           Sound.Types

saw :: Frequency -> Rate -> Audio
saw freq rate =
  let saw' t = 2 * (t*freq - fromIntegral (floor (1/2 + t*freq) :: Int))
  in fmap saw' (time rate)
{-# INLINE saw #-}
