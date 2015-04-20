{-# LANGUAGE ScopedTypeVariables #-}
module Sound.Driver.Jack
  ( runAudio
  ) where

import           Control.Concurrent.MVar
import           Control.Monad.Trans
import           Control.Monad.Exception.Synchronous

import           Data.Array.Storable (writeArray)
import qualified Data.ByteString.Lazy as B

import           Foreign.C.Types (CFloat(..))
import           Foreign.C.Error(Errno)

import           GHC.Float

import           Sound.JACK (NFrames(..),Output,Input)
import           Sound.JACK.Audio (Port)
import qualified Sound.JACK.Audio as Audio
import qualified Sound.JACK as JACK
import qualified Sound.JACK.MIDI as MIDI

import qualified Data.Stream as S
import           Sound.Types

import           Music.Midi
import           Music.VoiceMap

type Synthesizer = MVar VoiceMap

runAudio :: VoiceMap -> IO ()
runAudio f = do
  sig <- newMVar f
  start "hsynth" sig

start :: String -> Synthesizer -> IO ()
start name synth = do
  JACK.handleExceptions $
      JACK.withClientDefault name $ \client ->
      MIDI.withPort client "input" $ \input ->
      JACK.withPort client "output" $ \output -> do
        rate <- lift $ JACK.getSampleRate client
        JACK.withProcess client (render synth input output rate) $
            JACK.withActivation client $ lift $ do
                putStrLn $ "started " ++ name ++ "..."
                JACK.waitForBreak

render :: Synthesizer -> MIDI.Port Input -> Port Output -> Rate -> NFrames -> ExceptionalT Errno IO ()
render synth input output rate nframes@(NFrames n) = do
  rawMidiEvents <- MIDI.readRawEventsFromPort input nframes
  let midiMsgs = concat $ map (getMessages . B.fromStrict . MIDI.rawEventBuffer) rawMidiEvents
  lift $ do
    {-print midiMsgs-}
    out <- Audio.getBufferArray output nframes
    modifyMVar_ synth $ \vm -> do
      let vm' = foldr (interpret rate) vm midiMsgs
      {-print vm'-}
      let (sample,vm'') = mapAccumNotes mixSample emptySample vm'
      write out 0 (if size vm'' == 0 then sample else map (/ fromIntegral (size vm'')) sample)
      return vm''
  where
    emptySample = replicate (fromIntegral n) 0

    mixSample :: [Double] -> Audio -> ([Double],Audio)
    mixSample sample audio = 
      let (sample',audio') = S.splitAt (fromIntegral n) audio
      in (zipWith (+) sample sample',audio')

    write out i (amp:samp) | i < n = do
      writeArray out (NFrames i) (double2CFloat amp)
      write out (i+1) samp
    write _ _ _ = return ()

double2CFloat :: Double -> CFloat
double2CFloat x = CFloat (double2Float x)
{-# INLINE double2CFloat #-}
