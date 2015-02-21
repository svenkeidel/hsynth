module Sound.Triangle where

import           Data.Fixed (mod')

import           Sound.Time
import           Sound.Types

triangle :: Frequency -> Rate -> Audio
triangle freq rate =
  let tri t = abs (4 * ((t / (2 * pi) * freq - 1/4) `mod'` 1) - 2) - 1
  in fmap tri (time rate)
{-# INLINE triangle #-}
