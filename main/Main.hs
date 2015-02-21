module Main where

import           Control.Applicative

import qualified Data.Stream as S

import qualified Sound.Driver.StdOut as Driver
import           Sound.BaseDrum
import           Sound.Sinus
import           Sound.Flute

import           System.Environment (getArgs)
import qualified System.Random as R


main = do
  {-(amp:freq:rt:_) <- (fmap read) <$> getArgs-}
  (freq:rt:_) <- (fmap read) <$> getArgs
  let rate = truncate rt
  Driver.runAudio16 (0.5 * kickDrum freq rate)
  {-Driver.runAudio16 (0.5 * oscSine freq rate (S.repeat 0))-}
  {-Driver.runAudio16 (0.5 * oscSine freq rate (sinA 0.01 rate))-}
  {-Driver.runAudio8 rate (lowpass c (0.5 * (triangle freq rate)))-}
  {-Driver.runAudio16 (0.5 * (sinA freq rate))-}
  {-g <- R.newStdGen-}
  {-Driver.runAudio16 (flute 5 0.5 440 0.9 0.02 g rate)-}
