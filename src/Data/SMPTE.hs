module Data.SMPTE
       ( SMPTETimeCode(..)
       , SMPTEFormat(..)
       , ticksPerSecond
       ) where

import Data.Word

type Hour = Word8
type Minute = Word8
type Second = Word8
type Frame = Word8
--type SubFrame = Word8
data SMPTETimeCode = SMPTETimeCode Hour Minute Second Frame
  deriving (Show,Eq)

type FramesPerSecond = Int
type SubframesPerFrame = Int
type TicksPerSecond = Int
data SMPTEFormat = SMPTEFormat FramesPerSecond SubframesPerFrame
  deriving (Show,Eq)

ticksPerSecond :: SMPTEFormat -> TicksPerSecond
ticksPerSecond (SMPTEFormat fps spf) = fps * spf
