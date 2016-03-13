{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Prelude (($))
import qualified Sound.Driver.Jack as Driver
import           Sound.Sine
import           Language.Frontend
import           Data.List

main = do
  Driver.runAudioFun $(compile (sinA 0.1 48000 >>> amp 2 >>> add 2 >>> oscSine 440 48000))
