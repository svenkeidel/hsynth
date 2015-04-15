module Music.Midi where

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

{-data ChannelModeMessage-}
  {-= -}
