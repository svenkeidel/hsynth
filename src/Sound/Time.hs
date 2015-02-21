module Sound.Time where

import           Data.Stream (Stream)
import qualified Data.Stream as S

import           Sound.Types

time :: Rate -> Stream Double
time rate = S.unfold (\n -> (fromIntegral n / fromIntegral rate,n+1)) (0 :: Int)
{-# INLINE time #-}
