module Main where

import           Control.Applicative

import qualified Data.Stream as S

import qualified Sound.Driver.Pulse as Driver
import qualified Sound.Driver.Alsa as Alsa
import           Sound.BaseDrum
import           Sound.FadeOut
import           Sound.Flute
import           Sound.Sample
import           Sound.Sinus
import           Sound.Types
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
  args <- getArgs
  vm <- newMVar VM.empty
  let rate = 44100
  let audio freq vel rate = Sample (0.20 * sinA freq rate) (fadeOut 0.1 rate)
  let sig pitch vel rate = audio (tuning pitch) vel rate

  forkIO $ Alsa.withRawMidi (head args) $ \midi -> 
    Alsa.withMessages 1 midi $ \msg ->
      modifyMVar_ vm $ \voiceMap ->
        return $ VM.interpret (\pitch vel -> sig pitch vel rate) msg voiceMap

  Driver.withPulse rate $ \pulse ->
    forever $ modifyMVar_ vm $ \voiceMap -> do
      let (sample,vm') = VM.mapAccumNotes mixSample mixSampleList emptySample voiceMap
      Driver.writeChunk pulse sample
      return vm'

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

    chunkSize = 100

    emptySample = replicate chunkSize 0

    mixSample :: [Double] -> Audio -> ([Double], Audio)
    mixSample sample audio = 
      let (sample',audio') = S.splitAt chunkSize audio
      in (zipWith (+) sample sample',audio')

    mixSampleList :: [Double] -> [Double] -> ([Double], [Double])
    mixSampleList sample audio =
      let (sample',audio') = splitAt chunkSize audio
      in (zipWith (+) sample (padding sample'),audio')

    padding :: [Double] -> [Double]
    padding l = l ++ replicate (chunkSize - length l) 0
