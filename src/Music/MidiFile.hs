{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
module Music.MidiFile where

import           Control.Monad
import           Control.Monad.State
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.SMPTE
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Word
import           Music.MidiMessage
import           Data.Ratio

type DeltaTime = Word32
type FractionalFrame = Word8
type NumMidiClocksMetroClick = Word8
type ThirtySecondNotesPerQuarter = Word8
type TicksPerFrame = Int

data MidiFormat = SingleTrack | MultiTrack | MultiSong
  deriving (Show,Eq)

data Tempo = TicksPerQuarterNote Int | SMPTE SMPTEFormat
  deriving (Show,Eq)

data MidiFile = MidiFile MidiFormat Tempo [Track]
  deriving (Show,Eq)

newtype Track = Track [(DeltaTime, TrackEvent)]
  deriving (Show,Eq)

data TrackEvent
  = MidiEvent MidiMessage
  | MetaEvent MetaEvent
  | SystemExclusiveStart BL.ByteString
  | SystemExclusiveContinue BL.ByteString
  | SystemExclusiveEnd BL.ByteString
  deriving (Show,Eq)

data MetaEvent
  = SequenceNumber Int
  | TextEvent Text
  | CopyrightNotice Text
  | SequenceTrackName Text
  | InstrumentName Text
  | Lyric Text
  | Marker Text
  | CuePoint Text
  | MidiChannelPrefix Int
  | EndOfTrack
  | SetTempo Int
  | SMPTEOffset SMPTETimeCode FractionalFrame
  | TimeSignature Rational NumMidiClocksMetroClick ThirtySecondNotesPerQuarter
  | KeySignature KeySignature Mode
  | SequencerSpecific BL.ByteString

  deriving (Show,Eq)

type KeySignature = Int

data Mode = Major | Minor
  deriving (Show,Eq)

getMidiFile :: Get MidiFile
getMidiFile = do
  skip (length "MThd")
  skip 4
  format <- getMidiFormat
  n <- fromIntegral <$> getWord16be
  t <- getTempo
  MidiFile format t <$> forM [0..n-1] (\i -> label ("parse midi track " ++ show i) $ getTrack i)

getMidiFormat :: Get MidiFormat
getMidiFormat = do
  format <- getWord16be
  return $ case format of
    0 -> SingleTrack
    1 -> MultiTrack
    2 -> MultiSong
    _ -> error "Unrecognized midi format"

getTempo :: Get Tempo
getTempo = do
  division <- getWord16be
  return $ case division .&. 0b1000000000000000 of
    0 -> TicksPerQuarterNote $ fromIntegral division
    _ -> SMPTE $ SMPTEFormat (fromIntegral (negate (fromIntegral (shiftR division 8) :: Int8)))
                             (fromIntegral (division .&. 0b0000000011111111))

getTrack :: Int -> Get Track
getTrack trackNumber = do
  skip (length "MTrk")
  trackLength <- getWord32be
  Track <$> isolate (fromIntegral trackLength) (getTrackEvents trackNumber)

getTrackEvents :: Int -> Get [(DeltaTime,TrackEvent)]
getTrackEvents trackNumber = evalStateT go None
  where
    go = do
      empty <- lift isEmpty
      if empty
        then return []
        else do
          event  <- getTrackEvent trackNumber
          events <- go
          return (event:events)

getTrackEvent :: Int -> StateT RunningStatus Get (DeltaTime,TrackEvent)
getTrackEvent trackNumber = do
  deltaTime <- lift getVariableLengthQuantity
  eventType <- lift $ lookAhead getWord8
  (deltaTime,) <$> case eventType of
    0xFF -> lift $ label "meta event" $ skip 1 >> MetaEvent <$> getMetaEvent trackNumber
    0xF0 -> lift $ label "system exclusive" $ getSystemExclusiveEvent
    0xF7 -> lift $ label "system exclusive" $ getSystemExclusiveEvent
    _    -> MidiEvent <$> getMessage

getMetaEvent :: Int -> Get MetaEvent
getMetaEvent trackNumber = do
  b1 <- getWord8
  case b1 of
    0x00 -> label "sequence number" $ do
      b2 <- getWord8
      case b2 of
        0x00 -> return $ SequenceNumber trackNumber
        0x02 -> do
          seqNum <- fromIntegral <$> getWord16be
          return $ SequenceNumber seqNum
        _ -> error "unrecognized sequence number"
    0x01 -> label "text event" $ TextEvent <$> getText
    0x02 -> label "copyright notice" $ CopyrightNotice <$> getText
    0x03 -> label "sequence/track name" $ SequenceTrackName <$> getText
    0x04 -> label "instrument name" $ InstrumentName <$> getText
    0x05 -> label "lyric" $ Lyric <$> getText
    0x06 -> label "marker" $ Marker <$> getText
    0x07 -> label "cue point" $ CuePoint <$> getText
    0x20 -> label "midi channel prefix" $ skip 1 >> MidiChannelPrefix <$> fromIntegral <$> getWord8
    0x2F -> label "end of track" $ skip 1 >> return EndOfTrack
    0x51 -> label "set tempo" $ do
      t <- getWord32be
      return $ SetTempo $ fromIntegral $ t .&. 0x00FFFFFF
    0x54 -> label "smpte time code" $ do
      skip 1
      timeCode <- liftM4 SMPTETimeCode getWord8 getWord8 getWord8 getWord8
      fractionalFrame <- getWord8
      return $ SMPTEOffset timeCode fractionalFrame
    0x58 -> label "time signature" $ do
      skip 1
      n <- fromIntegral <$> getWord8
      d <- fromIntegral <$> getWord8 :: Get Int
      TimeSignature (n % 2^d) <$> getWord8 <*> getWord8
    0x59 -> label "key signature" $ do
      skip 1
      sf <- fromIntegral <$> getWord8
      mi <- getWord8
      return $ KeySignature sf $ case mi of
        0 -> Major
        1 -> Minor
        _ -> error "unrecognized key mode"
    0x7F -> label "sequencer specific" $ do
      n <- fromIntegral <$> getVariableLengthQuantity
      SequencerSpecific <$> BL.fromStrict <$> getByteString n
    _ -> error "unrecognized meta event"

getText :: Get Text
getText = do
  len <- fromIntegral <$> getVariableLengthQuantity
  text <- getByteString len
  return $ decodeUtf8With (\_ _ -> Nothing) text

getSystemExclusiveEvent :: Get TrackEvent
getSystemExclusiveEvent = do
  typ <- getWord8
  len <- fromIntegral <$> getVariableLengthQuantity
  case typ of
    0xF0 -> SystemExclusiveStart <$> BL.fromStrict <$> getByteString len
    0xF7 -> do
      dat <- BL.fromStrict <$> getByteString (len - 1)
      last' <- getWord8
      case last' of
        0xF7 -> return $ SystemExclusiveEnd dat
        _    -> return $ SystemExclusiveContinue (dat `BL.snoc` last')
    _ -> error "Unrecognized system exclusive event"

getVariableLengthQuantity :: Get Word32
getVariableLengthQuantity = go 0
  where
    go acc = do
      w <- getWord8
      let stop = w .&. 0b10000000 == 0
          sevenBitData = w .&. 0b01111111
          acc' = shiftL acc 7 + fromIntegral sevenBitData
      if stop
         then return acc'
         else go acc'
