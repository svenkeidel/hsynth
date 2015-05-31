{-# LANGUAGE BinaryLiterals #-}
module Sound.Driver.SDL where

import qualified Graphics.UI.SDL as SDL
import           Data.Bits ((.|.))
import           Data.Int
import           Data.Stream (Stream)
import qualified Data.Stream as S
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           Foreign.Ptr
import           Foreign.Marshal.Array
import           Data.Char (chr,ord)
import           Data.Word
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import           Data.ByteString.Builder.Extra (Next)
import qualified Data.ByteString.Builder.Extra as B

import           Music.Midi

while :: IO Bool -> IO ()
while action = do
  continue <- action
  if continue
    then while action
    else return ()

eventLoop :: (SDL.Event -> IO Bool) -> IO ()
eventLoop handler = do
  alloca $ \event -> while $ do
    isAvailable <- SDL.pollEvent event
    if isAvailable /= 0
      then do 
        e <- peek event 
        handler e
      else
        return True

data KeyEvent = KeyDown Char | KeyUp Char
  deriving Show

keySymToEvent :: Word32 -> SDL.Keysym -> Maybe KeyEvent
keySymToEvent typ (SDL.Keysym _ c _) =
  let evType = case typ of
        SDL.SDL_KEYDOWN -> KeyDown
        SDL.SDL_KEYUP -> KeyUp
        _ -> error "unknown keyevent"
  in if inRange
      then Just $ evType (chr (fromIntegral c))
      else Nothing
  where
    inRange = fromIntegral c >= ord (minBound :: Char)
           && fromIntegral c <= ord (maxBound :: Char)

keyEventToMidi :: KeyEvent -> Maybe MidiMessage
keyEventToMidi ev = Voice 0 <$> case ev of
  KeyDown c -> NoteOn  <$> charToPitch c <*> pure 127
  KeyUp c   -> NoteOff <$> charToPitch c <*> pure 127

charToPitch :: Char -> Maybe Word8
charToPitch char = 
  case char of
    's' -> j 1; 'd' -> j 3;             'g' -> j 6; 'h' -> j 8; 'j' -> j 10;
    'z' -> j 0; 'x' -> j 2; 'c' -> j 4; 'v' -> j 5; 'b' -> j 7; 'n' -> j 9; 'm' -> j 11
    _ -> Nothing
  where
    j = Just

take :: Int -> Audio -> (Builder,Audio)
take n s =
  let (front,rest) = S.splitAt n s
  in (foldMap (B.int32LE . quantizeSigned32) front,rest)

copyToBuffer :: Builder -> Ptr Word8 -> Int -> IO ()
copyToBuffer builder buf0 len0 = do
  cont <- B.runBuilder builder buf0 len0
  go cont buf0 len0
  where
    go :: (Int,Next) -> Ptr Word8 -> Int -> IO ()
    go (bytesWritten,next) buf len =
      case next of
        B.Done      -> return ()
        B.More _ k  -> do
          let buf' = advancePtr buf bytesWritten
              len' = len - bytesWritten
          cont <- k buf' len'
          go cont buf' len'
        B.Chunk _ k -> do
          let buf' = advancePtr buf bytesWritten
              len' = len - bytesWritten
          cont <- k buf' len'
          go cont buf' len'


withSDL :: (KeyEvent -> IO ()) -> (Ptr Word8 -> Int -> IO ()) -> IO ()
withSDL handle audioCb = do
  _ <- SDL.init (SDL.SDL_INIT_VIDEO .|. SDL.SDL_INIT_EVENTS .|. SDL.SDL_INIT_AUDIO)

  _ <- withCString "hsynth" $ \hsynth -> SDL.createWindow hsynth 0 0 600 400 0


  cb <- SDL.mkAudioCallback $ (\_ buf len -> audioCb buf (fromIntegral len))

  let audio_s32_le = 0b1000000000100000
  let audioSpec = SDL.AudioSpec
        { SDL.audioSpecFreq = 48000
        , SDL.audioSpecFormat = audio_s32_le
        , SDL.audioSpecChannels = 1 
        , SDL.audioSpecSilence = 0
        , SDL.audioSpecSamples = 1024
        , SDL.audioSpecSize = 0
        , SDL.audioSpecCallback = cb
        , SDL.audioSpecUserdata = nullPtr
        }

  alloca $ \want ->
    alloca $ \have -> do
      poke want audioSpec
      _ <- SDL.openAudio want have
      SDL.pauseAudio 0 
      return ()

  eventLoop $ \e -> do
    case e of
      (SDL.QuitEvent _ _) -> return False
      (SDL.KeyboardEvent evType _ _ _ _ sym) -> do
        mapM_ handle (keySymToEvent evType sym)
        return True
      _ -> return True

  SDL.closeAudio
  SDL.quit
