{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
module Music.MidiMessage where

import           Control.Monad.State

import           Data.Word
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy.Internal (chunk,ByteString(..))
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BSB
import           Data.Monoid ((<>))

import           Music.MidiManufacturer

data MidiMessage
  = Voice Channel ChannelVoiceMessage
  | Mode ChannelModeMessage
  | SystemCommon SystemCommonMessage
  | SystemExclusive Manufacturer ByteString
  | SystemRealTime SystemRealTimeMessage
  | ParseError String
  | Reserved
  deriving (Show,Eq)

type Channel = Word8
type Pitch = Word8
type Velocity = Word8
type Controller = Word8
type ControllerValue = Word8
type Program = Word8
type Pressure = Word8
type PitchBend = Int
type NumberOfChannels = Int
type Nibble = Word8
type Song = Word8

data ChannelVoiceMessage
  = NoteOff Pitch Velocity
  | NoteOn Pitch Velocity
  | PolyphonicKeyPressure Pitch Velocity
  | ControlChange Controller ControllerValue
  | ProgramChange Program
  | ChannelPressure Pressure
  | PitchBend PitchBend
  deriving (Show,Eq)

data RunningStatus
  = RunningStatus Channel ChannelVoiceMessage
  | None

data ChannelModeMessage
  = AllSoundOff
  | ResetAllControllers
  | LocalControlOff
  | LocalControlOn
  | AllNotesOff
  | OmniModeOff
  | OmniModeOn
  | MonoModeOn NumberOfChannels
  | PolyModeOn
  deriving (Show,Eq)

data SystemCommonMessage
  = TimeCodeQuarterFrame TimeCode Nibble
  | SongPosition Int
  | SongSelect Song
  | TuneRequest
  deriving (Show,Eq)

data TimeCode
  = FrameLow
  | FrameHigh
  | SecondLow
  | SecondHigh
  | MinuteLow
  | MinuteHigh
  | HourLow
  | HourHigh
  deriving (Show,Eq)

data SystemRealTimeMessage
  = TimeClock
  | Start
  | Continue
  | Stop
  | ActiveSensing
  | Reset
  deriving (Show,Eq)

getMessages :: ByteString -> [MidiMessage]
getMessages = go (decoder None)
  where
    decoder status = do
      runGetIncremental (runStateT getMessage status)

    go :: Decoder (MidiMessage,RunningStatus) -> ByteString -> [MidiMessage]
    go (Done leftover _ (msg,status)) input =
      msg : go (decoder status) (chunk leftover input)
    go (Partial k) input
      | BL.null input = []
      | otherwise     = go (k . takeHeadChunk $ input) (dropHeadChunk input)
    go (Fail _ _ errMsg) _ = error errMsg

getMessage :: StateT RunningStatus Get MidiMessage
getMessage = do
  (firstHalfWord,lastHalfWord,word) <- lift splitHalf
  let chan = lastHalfWord
      voiceMsg :: Get ChannelVoiceMessage -> StateT RunningStatus Get MidiMessage
      voiceMsg getMsg = do
        msg <- lift getMsg
        put (RunningStatus chan msg)
        return $ Voice chan msg
  case firstHalfWord of
    0b1000 -> voiceMsg $ NoteOff <$> getWord8 <*> getWord8
    0b1001 -> voiceMsg $ fixNoteOnOff <$> (NoteOn <$> getWord8 <*> getWord8)
    0b1010 -> voiceMsg $ PolyphonicKeyPressure <$> getWord8 <*> getWord8
    0b1011 -> getController chan
    0b1100 -> voiceMsg $ ProgramChange <$> getWord8
    0b1101 -> voiceMsg $ ChannelPressure <$> getWord8
    0b1110 -> voiceMsg $ PitchBend <$> fromIntegral <$> getWord16be
    0b1111 -> getSystemMessage lastHalfWord
    _ -> do
      RunningStatus chan' status <- get
      Voice chan' <$> case status of
        (NoteOff _ _)               -> lift $ NoteOff word <$> getWord8
        (NoteOn _ _)                -> lift $ fixNoteOnOff <$> NoteOn word <$> getWord8
        (PolyphonicKeyPressure _ _) -> lift $ PolyphonicKeyPressure word <$> getWord8
        (ProgramChange _)           -> return $ ProgramChange word
        (ChannelPressure _)         -> return $ ChannelPressure word
        (PitchBend _)               -> lift $ do
          msb <- fromIntegral <$> getWord8
          return $ PitchBend (fromIntegral word + 128 * msb)
        (ControlChange _ _)         -> lift $ ControlChange word <$> getWord8
  where
    -- NoteOn messages with a velocity of 0 are NoteOff events
    fixNoteOnOff :: ChannelVoiceMessage -> ChannelVoiceMessage
    fixNoteOnOff (NoteOn note 0) = NoteOff note 0
    fixNoteOnOff msg             = msg

getController :: Channel -> StateT RunningStatus Get MidiMessage
getController chan = do
  controller <- lift getWord8
  value <- lift getWord8
  case (controller,value) of
    (120,0)   -> return $ Mode AllSoundOff
    (121,_)   -> return $ Mode ResetAllControllers
    (122,0)   -> return $ Mode LocalControlOff
    (122,127) -> return $ Mode LocalControlOn
    (123,_)   -> return $ Mode AllSoundOff
    (124,_)   -> return $ Mode OmniModeOff
    (125,_)   -> return $ Mode OmniModeOn
    (126,_)   -> return $ Mode (MonoModeOn (fromIntegral value))
    (127,_)   -> return $ Mode PolyModeOn
    _         -> do
      let msg = ControlChange controller value
      put (RunningStatus chan msg)
      return $ Voice chan msg
{-# INLINE getController #-}

getSystemMessage :: Word8 -> StateT RunningStatus Get MidiMessage
getSystemMessage lastHalfWord =
  case lastHalfWord of
    0b0000 -> resetRunningStatus >> lift getSystemExclusiveMessage
    0b0001 -> resetRunningStatus >> lift getTimeCodeMessage
    0b0010 -> resetRunningStatus >> SystemCommon . SongPosition . fromIntegral <$> lift getWord16be
    0b0011 -> resetRunningStatus >> SystemCommon . SongSelect <$> lift getWord8
    0b0100 -> return $ Reserved
    0b0101 -> return $ Reserved
    0b1000 -> return $ SystemRealTime TimeClock
    0b1001 -> return $ Reserved
    0b1010 -> return $ SystemRealTime Start
    0b1011 -> return $ SystemRealTime Continue
    0b1100 -> return $ SystemRealTime Stop
    0b1101 -> return $ Reserved
    0b1110 -> return $ SystemRealTime ActiveSensing
    0b1111 -> return $ SystemRealTime Reset
    _      -> lift $ parseError $ "unknown midi system message: (" ++ show (0b11111 :: Word8) ++ "," ++ show lastHalfWord ++ ")"
  where
    resetRunningStatus = put None
{-# INLINE getSystemMessage #-}

getTimeCodeMessage :: Get MidiMessage
getTimeCodeMessage = do
  word <- getWord8
  let messageType = shiftR (word .&. 0b01110000) 4
      nibble = word .&. 0b00001111
      timeCode = case messageType of
        0b0000 -> Right FrameLow
        0b0001 -> Right FrameHigh
        0b0010 -> Right SecondLow
        0b0011 -> Right SecondHigh
        0b0100 -> Right MinuteLow
        0b0101 -> Right MinuteHigh
        0b0110 -> Right HourLow
        0b0111 -> Right HourHigh
        _      -> Left $ "unknown time code: " ++ show messageType
  return $ either ParseError (\tc -> SystemCommon $ TimeCodeQuarterFrame tc nibble) timeCode
{-# INLINE getTimeCodeMessage #-}

getSystemExclusiveMessage :: Get MidiMessage
getSystemExclusiveMessage = SystemExclusive <$> getManufacturer <*> takeUntil 0b11110111
{-# INLINE getSystemExclusiveMessage #-}

getManufacturer :: Get Manufacturer
getManufacturer = getManufacturer1Byte
{-# INLINE getManufacturer #-}


-- Helper Methods

-- | Takes bytes until a terminator is reached. The terminator is *not* included
-- in the result
takeUntil :: Word8 -> Get ByteString
takeUntil terminator = go mempty
  where
    go :: Builder -> Get ByteString
    go builder = do
      word <- getWord8
      if word == terminator
        then return $ BSB.toLazyByteString builder
        else go $ builder <> BSB.word8 word
{-# INLINE takeUntil #-}

parseError :: String -> Get MidiMessage
parseError = return . ParseError
{-# INLINE parseError #-}

-- | Splits a word into its first four bits and its last four bits.
splitHalf :: Get (Word8,Word8,Word8)
splitHalf = do
  word <- getWord8
  return (shiftR word 4, word .&. 0b00001111,word)
{-# INLINE splitHalf #-}


takeHeadChunk :: ByteString -> Maybe BS.ByteString
takeHeadChunk lbs =
  case lbs of
    (Chunk bs _) -> Just bs
    _ -> Nothing
{-# INLINE takeHeadChunk #-}

dropHeadChunk :: ByteString -> ByteString
dropHeadChunk lbs =
  case lbs of
    (Chunk _ lbs') -> lbs'
    _ -> Empty
{-# INLINE dropHeadChunk #-}
