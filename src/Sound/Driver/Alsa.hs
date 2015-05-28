{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.Driver.Alsa where

import           Foreign.Ptr
import           Foreign.C.String
import           Foreign.C.Types
import           Control.Exception (bracket)

import           Data.Binary.Get (Decoder)
import qualified Data.Binary.Get as G

import qualified Data.ByteString as BS

import           Music.Midi

data RawMidiStruct
type RawMidiHandle = Ptr RawMidiStruct

openRawMidi :: String -> IO RawMidiHandle
openRawMidi device = withCString device open_rawmidi

withRawMidi :: String -> (RawMidiHandle -> IO ()) -> IO ()
withRawMidi device = bracket (openRawMidi device) close_rawmidi

type Buffer = CString

withMessages :: Int -> RawMidiHandle -> (MidiMessage -> IO ()) -> IO ()
withMessages size handle onMessage = withCString (replicate size ' ') $ go decoder
  where
    decoder = G.runGetIncremental getMessage

    go :: Decoder MidiMessage -> Buffer -> IO ()
    go (G.Done leftover _ msg) buffer = do
      onMessage msg
      read_rawmidi handle buffer (fromIntegral size)
      rest <- BS.packCString buffer
      go (G.pushChunk (G.pushChunk decoder leftover) rest) buffer
    go (G.Partial k) buffer = do
      read_rawmidi handle buffer (fromIntegral size)
      rest <- BS.packCString buffer
      go (k (Just rest)) buffer
    go (G.Fail _ _ errMsg) _ = fail errMsg

foreign import ccall "alsa.c open_rawmidi" open_rawmidi :: CString -> IO RawMidiHandle
foreign import ccall "alsa.c close_rawmidi" close_rawmidi :: RawMidiHandle -> IO ()
foreign import ccall "alsa.c snd_rawmidi_read" read_rawmidi :: RawMidiHandle -> Buffer -> CSize -> IO ()
