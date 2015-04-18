module Music.VoiceMap
( VoiceMap
, voiceMap
, noteOn
, noteOff
, mapAccumNotes
, size
, interpret
) where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as M
import           Music.Midi (Pitch,Velocity,MidiMessage(..),ChannelVoiceMessage(..))
import           Sound.Types

-- | Maintains audio signals of a specific MIDI pitch
data VoiceMap = VoiceMap
  { signal :: Pitch -> Velocity -> Rate -> Audio
  , voices :: IntMap Audio
  }

instance Show VoiceMap where
  show vm = show $ M.keys (voices vm)

voiceMap :: (Pitch -> Velocity -> Rate -> Audio) -> VoiceMap
voiceMap sig = VoiceMap sig M.empty
{-# INLINE voiceMap #-}

noteOn :: Pitch -> Velocity -> Rate -> VoiceMap -> VoiceMap
noteOn pitch vel rate vm =
  insertNote pitch (signal vm pitch vel rate) vm
{-# INLINE noteOn #-}

noteOff :: Pitch -> VoiceMap -> VoiceMap
noteOff = deleteNote
{-# INLINE noteOff #-}

size :: VoiceMap -> Int
size = M.size . voices
{-# INLINE size #-}

interpret :: Rate -> MidiMessage -> VoiceMap -> VoiceMap
interpret rate (Voice _ (NoteOn pitch vel)) vm =
  noteOn pitch vel rate vm
interpret _ (Voice _ (NoteOff pitch _)) vm =
  noteOff pitch vm
-- ingore all other messages
interpret _ _ vm = vm
{-# INLINE interpret #-}

mapAccumNotes :: (a -> Audio -> (a,Audio)) -> a -> VoiceMap -> (a, VoiceMap)
mapAccumNotes f a vm =
  let (a',voices') = M.mapAccum f a (voices vm)
  in (a',vm { voices = voices' })
{-# INLINE mapAccumNotes #-}

insertNote :: Pitch -> Audio -> VoiceMap -> VoiceMap
insertNote pitch audio vm =
  vm { voices = M.insert (fromIntegral pitch) audio (voices vm) }
{-# INLINE insertNote #-}

deleteNote :: Pitch -> VoiceMap -> VoiceMap
deleteNote pitch vm =
  vm { voices = M.delete (fromIntegral pitch) (voices vm) }
{-# INLINE deleteNote #-}
