module Main where

import           Control.Applicative

import qualified Data.Stream as S

import qualified Sound.Driver.Jack as Driver
import           Sound.BaseDrum
import           Sound.FadeOut
import           Sound.Flute
import           Sound.Sample
import           Sound.Sine
import           Sound.Types
import           Sound.Quantization

import           Music.Pitch
import           Music.Tuning.EqualTemperament
import           Music.ConcertPitch (a440)
import           Music.Midi (Velocity)
import qualified Music.VoiceMap as VM

import           System.Environment (getArgs)
import qualified System.Random as R

import           Control.Monad(forever)

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar

main = do
  {-str <- newMVar (quantizeSigned16 <$> 0.5 * sinA 440 48000)-}
  {-Driver.withSDL (\_ _ -> return ()) $ \buf len ->-}
    {-modifyMVar_ str (Driver.store buf len)-}

  {-args <- getArgs-}
  {-vm <- newMVar VM.empty-}

  {-let audio freq vel rate = Sample (0.20 * sinA freq rate) (fadeOut 0.1 rate)-}
  {-let sig pitch vel rate = audio (tuning pitch) vel rate-}
  {-Driver.runAudio sig-}

  Driver.runAudio (0.5 * sinA 440 48000)

  {-audio freq vel rate = fromIntegral vel / fromIntegral (maxBound :: Velocity) * sinA freq rate-}
  {-audio freq vel rate = 0.5 * sinA freq rate-}
  {-let audio freq vel rate = Sample (0.10 * sinA freq rate) (fadeOut 0.5 rate)-}
  {-let sig pitch vel rate = audio (tuning pitch) vel rate-}
  {-Driver.runAudio16  (kickDrum 100 44100)-}
  {-Driver.runAudio16 (0.5 * oscSine freq rate (S.repeat 0))-}
  {-Driver.runAudio16 (0.5 * oscSine freq rate (sinA 0.01 rate))-}
  {-Driver.runAudio8 rate (lowpass c (0.5 * (triangle freq rate)))-}
  {-Driver.runAudio16 (0.5 * (sinA freq rate))-}
  {-Driver.runAudio16 (flute 5 0.5 440 0.9 0.02 g rate)-}
  where
    tuning = equalTemperament twelveTET a440 . fromMidiPitch
