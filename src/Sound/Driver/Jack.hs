{-# LANGUAGE ScopedTypeVariables #-}
module Sound.Driver.Jack
  ( runAudio
  ) where

import           Control.Concurrent.MVar
import           Control.Monad.Trans
import           Control.Monad.Exception.Synchronous

import           Data.Array.Storable (writeArray)

import           Foreign.C.Types (CFloat(..))
import           Foreign.C.Error(Errno)

import           GHC.Float

import           Sound.JACK (NFrames(..),Output)
import           Sound.JACK.Audio (Port)
import qualified Sound.JACK.Audio as Audio
import qualified Sound.JACK as JACK

import           Data.Stream (Stream(..))
import           Sound.Types

type AudioSig = MVar Audio

runAudio :: Audio -> IO ()
runAudio f = do
  sig <- newMVar f
  start "hsynth" sig

start :: String -> AudioSig -> IO ()
start name signal = do
  JACK.handleExceptions $
      JACK.withClientDefault name $ \client ->
      JACK.withPort client "output" $ \output ->
      JACK.withProcess client (render signal output) $
          JACK.withActivation client $ lift $ do
              putStrLn $ "started " ++ name ++ "..."
              JACK.waitForBreak

render :: AudioSig -> Port Output -> NFrames -> ExceptionalT Errno IO ()
render signal output nframes@(NFrames n) = lift $ do
  out <- Audio.getBufferArray output nframes
  modifyMVar_ signal $ write out 0
  where
    write out i (Cons amp sig) | i < n = do
      writeArray out (NFrames i) (double2CFloat amp)
      write out (i+1) sig
    write _   _ sig = return sig

double2CFloat :: Double -> CFloat
double2CFloat x = CFloat (double2Float x)
{-# INLINE double2CFloat #-}

{-mapMidi :: [Message.T] -> [Event]-}
{-mapMidi ((Message.Channel (Channel.Cons _ (Channel.Voice (Voice.NoteOn pitch velocity)))):msgs) = -}
  {-NoteOn (Voice.fromPitch pitch) (Voice.fromVelocity velocity) : mapMidi msgs-}
{-mapMidi ((Message.Channel (Channel.Cons _ (Channel.Voice (Voice.NoteOff pitch velocity)))):msgs) = -}
  {-NoteOff (Voice.fromPitch pitch) (Voice.fromVelocity velocity) : mapMidi msgs-}
{-mapMidi (_:msgs) = mapMidi msgs-}
{-mapMidi [] = []-}
