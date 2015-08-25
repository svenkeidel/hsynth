module Main where

import qualified Sound.Driver.Jack as Driver
import           Sound.Sine

main = do
  Driver.runAudio (0.1 * sinA 440 48000)