module Main where

import qualified Sound.Driver.StdOut as Driver
import           Sound.Sine

main = do
  Driver.runAudio (0.5 * sinA 440 48000)