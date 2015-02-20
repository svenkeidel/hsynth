module Sound.Lowpass where

import           Data.Stream ((<:>))
import qualified Data.Stream as S

import           Sound.Types

lowpass :: Double -> Audio -> Audio
lowpass c input =
  let output = 0 <:> S.zipWith (\i o -> o + c * (i - o)) input output
  in output
{-# INLINE lowpass #-}
