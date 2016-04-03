{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Prelude (($))
import qualified Sound.Driver.Jack as Driver
import           Sound.Amplifier
import           Sound.Saw
import           Language.Frontend
import           Data.List

main = do
  Driver.runAudioFun $(compile (saw 440 48000 >>> amp 0.1))
