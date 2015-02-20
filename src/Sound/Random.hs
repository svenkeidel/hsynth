module Sound.Random where

import qualified Data.Stream as S

import           Sound.Types

import qualified System.Random as R
import           System.Random (RandomGen)

random :: RandomGen g => g -> Audio
random = S.unfold (R.randomR (-1,1))
{-# INLINE random #-}

