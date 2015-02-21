module Sound.Triangle where

import           Sound.Saw
import           Sound.Types

triangle :: Frequency -> Rate -> Audio
triangle freq rate = fmap (\s -> 2 * abs s - 1) (saw freq rate)
{-# INLINE triangle #-}
