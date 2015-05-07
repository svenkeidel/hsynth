module Main where

import           Control.Applicative

import qualified Data.Stream as S

{-import qualified Sound.Driver.StdOut as Driver-}
import qualified Sound.Driver.Jack as Driver
import           Sound.BaseDrum
import           Sound.FadeOut
import           Sound.Flute
import           Sound.Sample
import           Sound.Sinus
import           Music.Pitch
import           Music.Tuning.EqualTemperament
import           Music.ConcertPitch (a440)
import           Music.Midi (Velocity)

import           System.Environment (getArgs)
import qualified System.Random as R


main = do
  {-audio freq vel rate = fromIntegral vel / fromIntegral (maxBound :: Velocity) * sinA freq rate-}
  {-audio freq vel rate = 0.5 * sinA freq rate-}
  let audio freq vel rate = Sample (0.10 * sinA freq rate) (fadeOut 0.5 rate)
      sig pitch vel rate = audio (tuning pitch) vel rate
  {-Driver.runAudio16  (kickDrum 100 44100)-}
  Driver.runAudio sig
  {-Driver.runAudio16 (0.5 * oscSine freq rate (S.repeat 0))-}
  {-Driver.runAudio16 (0.5 * oscSine freq rate (sinA 0.01 rate))-}
  {-Driver.runAudio8 rate (lowpass c (0.5 * (triangle freq rate)))-}
  {-Driver.runAudio16 (0.5 * (sinA freq rate))-}
  {-Driver.runAudio16 (flute 5 0.5 440 0.9 0.02 g rate)-}
  where
    tuning = equalTemperament twelveTET a440 . fromMidiPitch
