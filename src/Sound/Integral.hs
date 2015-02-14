module Sound.Integral where

import           Data.Stream (Stream)
import qualified Data.Stream as S

import           Sound.Types

integral :: Rate -> Stream Double -> Stream Double
integral rate = S.scan (\i x -> i + x * dt) 0
  where
    dt = 1 / fromIntegral rate
{-# INLINE integral #-}
