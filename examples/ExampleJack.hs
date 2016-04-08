{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Prelude (($))
import qualified Sound.Driver.Jack as Driver
import           Sound.Amplifier
import           Sound.Saw
import           Sound.Sine
import           Language.Frontend
import           Data.List

main = do
  Driver.runAudioFun $(compile ((mix (>+<) (saw 100 48000) (sinA 100 48000)) >>> amp 0.1))
