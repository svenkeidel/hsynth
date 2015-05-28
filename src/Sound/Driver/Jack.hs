{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Sound.Driver.Jack
  ( runAudio
  ) where

import           Control.Concurrent.MVar
import           Control.Monad.Trans
import           Control.Monad.Exception.Synchronous

import           Data.Array.Storable (writeArray)
import qualified Data.ByteString.Lazy as B
import           Data.Vector.Primitive (Vector)
import qualified Data.Vector.Primitive as V

import           Foreign.C.Types (CFloat(..))
import           Foreign.C.Error(Errno)

import           GHC.Float

import           Sound.JACK (NFrames(..),Output,Input)
import           Sound.JACK.Audio (Port)
import qualified Sound.JACK.Audio as Audio
import qualified Sound.JACK as JACK
import qualified Sound.JACK.MIDI as MIDI
import           Sound.Sample
import           Sound.Types

import qualified Data.Stream as S

import           Music.Midi
import           Music.VoiceMap (VoiceMap)
import qualified Music.VoiceMap as VM

type Synthesizer = Pitch -> Velocity -> Rate -> Sample

runAudio :: Synthesizer -> IO ()
runAudio synth = do
  sig <- newMVar VM.empty
  start "hsynth" synth sig

start :: String -> Synthesizer -> MVar VoiceMap -> IO ()
start name synth vm = do
  JACK.handleExceptions $
      JACK.withClientDefault name $ \client ->
      {-MIDI.withPort client "input" $ \input ->-}
      JACK.withPort client "output" $ \output -> do
        rate <- lift $ JACK.getSampleRate client
        JACK.withProcess client (render synth vm {-input-} output rate) $
            JACK.withActivation client $ lift $ do
                putStrLn $ "started " ++ name ++ "..."
                JACK.waitForBreak

render :: Synthesizer -> MVar VoiceMap -> {-MIDI.Port Input ->-} Port Output -> Rate -> NFrames -> ExceptionalT Errno IO ()
render synth mvm {-input-} output rate nframes@(NFrames n) = do
  {-rawMidiEvents <- MIDI.readRawEventsFromPort input nframes-}
  {-let midiMsgs = concat $ map (getMessages . B.fromStrict . MIDI.rawEventBuffer) rawMidiEvents-}
  lift $ do
    {-printMidiMsg $ filterMidiMsg midiMsgs-}
    print "called"
    {-putStrLn (unwords [ "rate", show rate-}
                      {-, "frames", show n-}
                      {-])-}
    out <- Audio.getBufferArray output nframes
    {-modifyMVar_ mvm $ \vm -> do-}
      {-let vm' = foldr (VM.interpret (\pitch vel -> synth pitch vel rate)) vm midiMsgs-}
      {-let (sample,vm'') = VM.mapAccumNotes mixSample mixSampleList emptySample vm'-}
      {-write out 0 (V.toList sample)-}
      {-return vm''-}
    write out 0 (repeat 0 :: [Double])
  where
    {-emptySample = V.replicate (fromIntegral n) 0-}

    {-filterMidiMsg :: [MidiMessage] -> [MidiMessage]-}
    {-filterMidiMsg = filter (\msg -> case msg of SystemRealTime _ -> False; _ -> True)-}

    {-printMidiMsg :: [MidiMessage] -> IO ()-}
    {-printMidiMsg [] = return ()-}
    {-printMidiMsg l = print l-}

    {-mixSample :: Vector Double -> Audio -> (Vector Double, Audio)-}
    {-mixSample sample audio = -}
      {-let (sample',audio') = S.splitAt (fromIntegral n) audio-}
      {-in (V.zipWith (+) sample (V.fromList sample'),audio')-}

    {-mixSampleList :: Vector Double -> [Double] -> (Vector Double, [Double])-}
    {-mixSampleList sample audio =-}
      {-let (sample',audio') = splitAt (fromIntegral n) audio-}
      {-in (V.zipWith (+) sample (V.fromList sample'),audio')-}

    write out i (amp:samp) | i < n = do
      writeArray out (NFrames i) (double2CFloat amp)
      write out (i+1) samp
    write _ _ _ = return ()

double2CFloat :: Double -> CFloat
double2CFloat x = CFloat (double2Float x)
{-# INLINE double2CFloat #-}
