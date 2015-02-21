module Sound.Random where

import qualified Data.Stream as S

import           Sound.Types
import           Sound.Integral

import qualified System.Random as R
import           System.Random (RandomGen)

random :: RandomGen g => g -> Audio
random = S.unfold (R.randomR (-1,1))
{-# INLINE random #-}

whiteNoise :: RandomGen g => g -> Audio
whiteNoise = random
{-# INLINE whiteNoise #-}

brownNoise :: RandomGen g => g -> Rate -> Audio
brownNoise g rate = integral rate (whiteNoise g)
{-# INLINE brownNoise #-}
