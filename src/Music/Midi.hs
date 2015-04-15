module Music.Midi where

import           Control.Applicative

import           Data.Word
import           Data.Binary (Get)
import qualified Data.Binary as B
import           Data.Bits

data MidiMessage
  = Voice Channel ChannelVoiceMessage
  | Mode ChannelModeMessage

type Channel = Word8
type Pitch = Word8
type Velocity = Word8
type Controller = Word8
type ControllerValue = Word8
type Program = Word8
type Pressure = Word8
type PitchBend = Int

data ChannelVoiceMessage
  = NoteOff Pitch Velocity
  | NoteOn Pitch Velocity
  | PolyphonicKeyPressure Pitch Velocity
  | ControlChange Controller ControllerValue
  | ProgramChange Program
  | ChannelPressure Pressure
  | PitchBend PitchBend

data ChannelModeMessage
  = AllSoundOff
  | ResetAllControllers
  | LocalControlOff
  | LocalControlOn
  | AllNotesOff
  | OmniModeOff
  | OmniModeOn
  | MonoModeOn
  | PolyModeOn

getMessage :: Get MidiMessage
getMessage = do
  (status,chan) <- splitHalf
  case status of
    8  -> Voice chan <$> (NoteOff <$> B.getWord8 <*> B.getWord8)
    9  -> Voice chan <$> (NoteOn <$> B.getWord8 <*> B.getWord8)
    10 -> Voice chan <$> (PolyphonicKeyPressure <$> B.getWord8 <*> B.getWord8)
    11 -> do
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
        (126,_)   -> Mode MonoModeOn
        (127,_)   -> Mode PolyModeOn
        _         -> Voice chan $ ControlChange controller value
    12 -> Voice chan <$> (ProgramChange <$> B.getWord8)
    13 -> Voice chan <$> (ChannelPressure <$> B.getWord8)
    14 -> do
      lsb <- B.getWord8
      msb <- B.getWord8
      return $ Voice chan $ PitchBend (fromIntegral (lsb + 128 * msb))

splitHalf :: Get (Word8,Word8)
splitHalf = do
  b <- B.getWord8
  return (shiftR b 4, b .&. (shiftL maxBound 4))
