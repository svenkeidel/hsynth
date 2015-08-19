module Main where

import qualified Data.Stream as S
import qualified Sound.Driver.Jack as Driver
import           Sound.Sine
import           Sound.FadeOut
import           Sound.Sample
import qualified Music.VoiceMap as VM
import           Music.Tuning.EqualTemperament
import           Music.ConcertPitch
import           Music.Pitch

main = do
  let rate = 48000
      tuning = equalTemperament twelveTET a440
      signal pitch = 0.1 * sinA (tuning pitch) rate
      sample _ pitch = Sample (signal (fromMidiPitch pitch)) (fadeOut 0.5 rate)
  Driver.runAudioWithMidi (\chunkSize -> S.unfoldR (VM.roll chunkSize) (VM.interpret sample) VM.empty)
