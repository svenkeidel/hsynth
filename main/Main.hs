module Main where

import           Control.Applicative

import qualified Data.Stream as S

{-import qualified Sound.Driver.StdOut as Driver-}
import qualified Sound.Driver.Jack as Driver
import           Sound.BaseDrum
import           Sound.Sinus
import           Sound.Flute
import           Music.Pitch
import           Music.Tuning.EqualTemperament
import           Music.ConcertPitch (a440)
import           Music.Midi (Velocity)
import           Music.VoiceMap (voiceMap)

import           System.Environment (getArgs)
import qualified System.Random as R


main = do
  Driver.runAudio (voiceMap sig)
  {-Driver.runAudio16 (0.5 * oscSine freq rate (S.repeat 0))-}
  {-Driver.runAudio16 (0.5 * oscSine freq rate (sinA 0.01 rate))-}
  {-Driver.runAudio8 rate (lowpass c (0.5 * (triangle freq rate)))-}
  {-Driver.runAudio16 (0.5 * (sinA freq rate))-}
  {-g <- R.newStdGen-}
  {-Driver.runAudio16 (flute 5 0.5 440 0.9 0.02 g rate)-}
  where
    sig pitch vel rate = audio (tuning pitch) vel rate
    {-audio freq vel rate = fromIntegral vel / fromIntegral (maxBound :: Velocity) * sinA freq rate-}
    audio freq vel rate = 0.5 * sinA freq rate
    tuning = equalTemperament twelveTET a440 . fromMidiPitch
