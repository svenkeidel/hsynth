{-# LANGUAGE BinaryLiterals #-}
module Music.Midi where

import           Data.Word
import           Data.Binary.Get (Get,Decoder)
import qualified Data.Binary.Get as B
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
getMessages = go decoder
  where
    decoder = B.runGetIncremental getMessage

    go :: Decoder MidiMessage -> ByteString -> [MidiMessage]
    go (B.Done leftover _ msg) input =
      msg : go decoder (chunk leftover input)
    go (B.Partial k) input
      | BL.null input = []
      | otherwise     = go (k . takeHeadChunk $ input) (dropHeadChunk input)
    go (B.Fail _ _ errMsg) _ = error errMsg
{-
instance Show a => Show (Decoder a) where
  show (B.Done leftover consumed a) = unwords ["Done", show leftover, show consumed, show a]
  show (B.Partial _) = "Partial _"
  show (B.Fail leftover consumed msg) = unwords ["Fail", show leftover, show consumed, msg]
-}

getMessage :: Get MidiMessage
getMessage = do
  (firstHalfWord,lastHalfWord) <- splitHalf
  let chan = lastHalfWord
  case firstHalfWord of
    0b1000 -> Voice chan <$> (NoteOff <$> B.getWord8 <*> B.getWord8)
    0b1001 -> Voice chan <$> (NoteOn <$> B.getWord8 <*> B.getWord8)
    0b1010 -> Voice chan <$> (PolyphonicKeyPressure <$> B.getWord8 <*> B.getWord8)
    0b1011 -> getController chan
    0b1100 -> Voice chan <$> (ProgramChange <$> B.getWord8)
    0b1101 -> Voice chan <$> (ChannelPressure <$> B.getWord8)
    0b1110 -> Voice chan <$> (PitchBend <$> getInt)
    0b1111 -> getSystemMessage lastHalfWord
    _      -> parseError $ "unknown midi message: (" ++ show firstHalfWord ++ "," ++ show lastHalfWord ++ ")"

getController :: Channel -> Get MidiMessage
getController chan = do
  controller <- B.getWord8
  value <- B.getWord8
  return $ case (controller,value) of
    (120,0)   -> Mode AllSoundOff
    (121,_)   -> Mode ResetAllControllers
    (122,0)   -> Mode LocalControlOff
    (122,127) -> Mode LocalControlOn
    (123,_)   -> Mode AllSoundOff
    (124,_)   -> Mode OmniModeOff
    (125,_)   -> Mode OmniModeOn
    (126,_)   -> Mode (MonoModeOn (fromIntegral value))
    (127,_)   -> Mode PolyModeOn
    _         -> Voice chan $ ControlChange controller value
{-# INLINE getController #-}

getSystemMessage :: Word8 -> Get MidiMessage
getSystemMessage lastHalfWord =
  case lastHalfWord of
    0b0000 -> getSystemExclusiveMessage
    0b0001 -> getTimeCodeMessage
    0b0010 -> SystemCommon <$> (SongPosition <$> getInt)
    0b0011 -> SystemCommon <$> (SongSelect <$> B.getWord8)
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
    _      -> parseError $ "unknown midi system message: (" ++ show (0b11111 :: Word8) ++ "," ++ show lastHalfWord ++ ")"
{-# INLINE getSystemMessage #-}

getTimeCodeMessage :: Get MidiMessage
getTimeCodeMessage = do
  word <- B.getWord8
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
      word <- B.getWord8
      if word == terminator
        then return $ BSB.toLazyByteString builder
        else go $ builder <> BSB.word8 word
{-# INLINE takeUntil #-}

getInt :: Get Int
getInt = do
  lsb <- fromIntegral <$> B.getWord8
  msb <- fromIntegral <$> B.getWord8
  return $ (lsb + 128 * msb)
{-# INLINE getInt #-}

parseError :: String -> Get MidiMessage
parseError = return . ParseError
{-# INLINE parseError #-}

-- | Splits a word into its first four bits and its last four bits.
splitHalf :: Get (Word8,Word8)
splitHalf = do
  word <- B.getWord8
  return (shiftR word 4, word .&. 0b00001111)
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
