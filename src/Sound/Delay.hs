module Sound.Delay where

import           Data.Stream (Stream(..),(<:>))
import qualified Data.Stream as S

import           Sound.Types

delay1 :: a -> Stream a -> Stream a
delay1 = S.Cons
{-# INLINE CONLIKE delay1 #-}

delay :: Int -> Audio -> Audio
delay n = S.switch n (S.repeat 0)
{-# INLINE delay #-}

delayTime :: Duration -> Rate -> Audio -> Audio
delayTime duration rate = delay (truncate (duration * fromIntegral rate))
{-# INLINE delayTime #-}
