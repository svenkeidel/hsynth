module Main where

import           Control.Applicative

import qualified Data.Stream as S

import qualified Sound.Driver.StdOut as Driver
{-import qualified Sound.Driver.Jack as Driver-}
import           Sound.BaseDrum
import           Sound.Sinus
import           Sound.Flute
import           Music.Pitch
import           Music.Tuning.EqualTemperament
import           Music.ConcertPitch (a440)
import           Music.Midi (Velocity)

import           System.Environment (getArgs)
import qualified System.Random as R


main = do
  {-audio freq vel rate = fromIntegral vel / fromIntegral (maxBound :: Velocity) * sinA freq rate-}
  {-audio freq vel rate = 0.5 * sinA freq rate-}
  g <- R.newStdGen
  let audio freq vel rate = flute 5 0.5 freq 0.9 0.02 g rate
      {-sig pitch vel rate = audio (tuning pitch) vel rate-}
      {-vm = voiceMap sig-}
  {-Driver.runAudio16  (kickDrum 100 44100)-}
  Driver.runAudio16 (audio 440 100 44100)
  {-Driver.runAudio16 (0.5 * oscSine freq rate (S.repeat 0))-}
  {-Driver.runAudio16 (0.5 * oscSine freq rate (sinA 0.01 rate))-}
  {-Driver.runAudio8 rate (lowpass c (0.5 * (triangle freq rate)))-}
  {-Driver.runAudio16 (0.5 * (sinA freq rate))-}
  {-Driver.runAudio16 (flute 5 0.5 440 0.9 0.02 g rate)-}
  {-where-}
    {-tuning = equalTemperament twelveTET a440 . fromMidiPitch-}
