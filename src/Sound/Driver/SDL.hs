{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
module Sound.Driver.SDL where

import           Linear
import           SDL (KeyState,Keysym)
import qualified SDL as SDL

import           Data.Word
import           Data.Int
import           Data.Stream 
import qualified Data.Stream as S

import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Marshal.Array
import           Foreign.Storable

import           Unsafe.Coerce

store :: Ptr Word8 -> CInt -> Stream Int16 -> IO (Stream Int16)
store buf len = go 0
  where
    go :: Int -> Stream Int16 -> IO (Stream Int16)
    go i s@(Cons x xs)
      | i >= fromIntegral len = return s
      | otherwise = do
        pokeElemOff alignedBuf i x
        go (i+1) xs

    alignedBuf = unsafeCoerce buf

withSDL :: (KeyState -> Keysym -> IO ()) -> (Ptr Word8 -> CInt -> IO ())-> IO ()
withSDL keyHandler audioCB = do

  SDL.initialize [SDL.InitAudio, SDL.InitVideo, SDL.InitEvents]
  (device,spec) <- SDL.openAudioDevice $ SDL.OpenDeviceSpec
    { SDL.openDeviceFreq = SDL.Mandate 48000
    , SDL.openDeviceFormat = SDL.Mandate (SDL.AudioFormat SDL.SignedInteger 16 SDL.Native)
    , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
    , SDL.openDeviceSamples = 1024
    , SDL.openDeviceCallback = audioCB
    , SDL.openDeviceUsage = SDL.ForPlayback
    , SDL.openDeviceName = Nothing
    }

  print $ SDL.audioSpecFormat spec
  SDL.setAudioDevicePlaybackState device SDL.Play

  window <- SDL.createWindow "hsynth" $ SDL.defaultWindow

  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window
  screenSurfaceFormat <- SDL.surfaceFormat screenSurface
  black <- SDL.mapRGB screenSurfaceFormat (V3 minBound minBound minBound)
  SDL.fillRect screenSurface Nothing black
  SDL.updateWindowSurface window

  let loop = do
        event <- SDL.pollEvent
        case SDL.eventPayload <$> event of
          Nothing            -> loop
          Just SDL.QuitEvent -> return ()
          Just (SDL.KeyboardEvent _ _ s _ k) -> do
            keyHandler s k
            loop
          Just (SDL.WindowResized _ _) -> do
            screenSurface <- SDL.getWindowSurface window
            screenSurfaceFormat <- SDL.surfaceFormat screenSurface
            black <- SDL.mapRGB screenSurfaceFormat (V3 minBound minBound minBound)
            SDL.fillRect screenSurface Nothing black
            SDL.updateWindowSurface window
            loop
          Just e -> do
            print e
            loop
  loop

  SDL.setAudioDevicePlaybackState device SDL.Pause
  SDL.closeAudioDevice device
  SDL.quit

{-withSDL :: (KeyEvent -> IO ()) -> (Ptr Word8 -> Int -> IO ()) -> IO ()-}
{-withSDL handle audioCb = do-}
  {-_ <- SDL.init (SDL.SDL_INIT_VIDEO .|. SDL.SDL_INIT_EVENTS .|. SDL.SDL_INIT_AUDIO)-}

  {-_ <- withCString "hsynth" $ \hsynth -> SDL.createWindow hsynth 0 0 600 400 0-}

  {-cb <- SDL.mkAudioCallback $ (\_ buf len -> audioCb buf (fromIntegral len))-}

  {-let audio_s32_le = 0b1000000000100000-}
  {-let audioSpec = SDL.AudioSpec-}
        {-{ SDL.audioSpecFreq = 48000-}
        {-, SDL.audioSpecFormat = audio_s32_le-}
        {-, SDL.audioSpecChannels = 1 -}
        {-, SDL.audioSpecSilence = 0-}
        {-, SDL.audioSpecSamples = 1024-}
        {-, SDL.audioSpecSize = 0-}
        {-, SDL.audioSpecCallback = cb-}
        {-, SDL.audioSpecUserdata = nullPtr-}
        {-}-}

  {-alloca $ \want ->-}
    {-alloca $ \have -> do-}
      {-poke want audioSpec-}
      {-_ <- SDL.openAudio want have-}
      {-SDL.pauseAudio 0 -}
      {-return ()-}

  {-eventLoop $ \e -> do-}
    {-case e of-}
      {-(SDL.QuitEvent _ _) -> return False-}
      {-(SDL.KeyboardEvent evType _ _ _ _ sym) -> do-}
        {-mapM_ handle (keySymToEvent evType sym)-}
        {-return True-}
      {-_ -> return True-}

  {-SDL.closeAudio-}
  {-SDL.quit-}
