module Main where

import           Control.Applicative

import qualified Sound.Driver.StdOut as Driver
import qualified Sound.Sinus as S
import           Sound.Flute

import           System.Environment (getArgs)
import qualified System.Random as R


main = do
  (dur:amp:freq:pres:breath:rt:_) <- (fmap read) <$> getArgs
  let rate = truncate rt
  g <- R.newStdGen
  Driver.runAudio8 rate (flute dur amp freq pres breath g rate)
