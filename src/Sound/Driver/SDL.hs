{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
module Sound.Driver.SDL where

import           Linear
import           SDL (KeyState,Keysym)
import qualified SDL as SDL
{-import           Data.Bits-}
import           Data.Word

{-import           Data.Bits ((.|.))-}
{-import           Data.Int-}
{-import           Data.Stream (Stream)-}
{-import qualified Data.Stream as S-}
{-import           Foreign.C.String-}
{-import           Foreign.Marshal.Alloc-}
{-import           Foreign.Storable-}
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Marshal.Array
{-import           Data.Char (chr,ord)-}
{-import           Data.Word-}
{-import           Data.ByteString.Builder (Builder)-}
{-import qualified Data.ByteString.Builder as B-}
{-import           Data.ByteString.Builder.Extra (Next)-}
{-import qualified Data.ByteString.Builder.Extra as B-}

{-import           Sound.Quantization-}
{-import           Sound.Types-}
{-import           Music.Midi-}

{-take :: Int -> Audio -> (Builder,Audio)-}
{-take n s =-}
  {-let (front,rest) = S.splitAt n s-}
  {-in (foldMap (B.int32LE . quantizeSigned32) front,rest)-}

{-copyToBuffer :: Builder -> Ptr Word8 -> Int -> IO ()-}
{-copyToBuffer builder buf0 len0 = do-}
  {-cont <- B.runBuilder builder buf0 len0-}
  {-go cont buf0 len0-}
  {-where-}
    {-go :: (Int,Next) -> Ptr Word8 -> Int -> IO ()-}
    {-go (bytesWritten,next) buf len =-}
      {-case next of-}
        {-B.Done      -> return ()-}
        {-B.More _ k  -> do-}
          {-let buf' = advancePtr buf bytesWritten-}
              {-len' = len - bytesWritten-}
          {-cont <- k buf' len'-}
          {-go cont buf' len'-}
        {-B.Chunk _ k -> do-}
          {-let buf' = advancePtr buf bytesWritten-}
              {-len' = len - bytesWritten-}
          {-cont <- k buf' len'-}
          {-go cont buf' len'-}



withSDL :: (KeyState -> Keysym -> IO ()) -> (Ptr Word8 -> CInt -> IO ())-> IO ()
withSDL keyHandler audioCB = do
  input <- mallocArray 1024 :: IO (Ptr Word32)
  output <- mallocArray 1024 :: IO (Ptr Word8)

  SDL.initialize [SDL.InitAudio, SDL.InitVideo, SDL.InitEvents]
  (device,_) <- SDL.openAudioDevice $ SDL.OpenDeviceSpec
    { SDL.openDeviceFreq = SDL.Mandate 48000
    , SDL.openDeviceFormat = SDL.Mandate (SDL.AudioFormat SDL.SignedInteger 16 SDL.Native)
    , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
    , SDL.openDeviceSamples = 1024
    , SDL.openDeviceCallback = audioCB
    , SDL.openDeviceUsage = SDL.ForPlayback
    , SDL.openDeviceName = Nothing
    }

  window <- SDL.createWindow "hsynth" $ SDL.defaultWindow
    { SDL.windowResizable = True
    }

  screenSurface <- SDL.getWindowSurface window
  screenSurfaceFormat <- SDL.surfaceFormat screenSurface
  black <- SDL.mapRGB screenSurfaceFormat (V3 minBound minBound minBound)
  SDL.fillRect screenSurface Nothing black
  SDL.updateWindowSurface window
  SDL.showWindow window

  let loop = do
        event <- SDL.pollEvent
        case SDL.eventPayload <$> event of
          Nothing            -> loop
          Just SDL.QuitEvent -> return ()
          Just (SDL.KeyboardEvent _ _ s _ k) ->
            keyHandler s k
          Just (SDL.WindowResized _ _) -> do
            SDL.fillRect screenSurface Nothing black
            SDL.updateWindowSurface window
          Just e -> do
            print e
            loop
  loop

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
