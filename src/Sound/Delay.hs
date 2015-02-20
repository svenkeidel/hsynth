module Sound.Delay where

import qualified Data.Stream as S

import           Sound.Types

delay :: Duration -> Rate -> Audio -> Audio
delay duration rate = S.switch n (S.repeat 0)
  where
    n = truncate (duration * fromIntegral rate)
{-# INLINE delay #-}
