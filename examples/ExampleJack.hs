{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Prelude (IO)
import qualified Sound.Driver.Jack as Driver
import           Sound.Amplifier
import           Sound.Oscillators
import           Language.Frontend

main :: IO ()
main = do
  Driver.runAudioFun $(compile (square 440 48000 >>> amp 0.5))
