module Music.VoiceMap
( VoiceMap
, empty
, noteOn
, noteOff
, mapAccumNotes
, size
, interpret
) where

import           Control.Arrow (second)

import           Data.Maybe (fromMaybe)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as M

import           Music.Midi (Pitch,Velocity,MidiMessage(..),ChannelVoiceMessage(..))

import           Sound.Sample
import           Sound.Types

-- | Maintains audio signals of a specific MIDI pitch
data VoiceMap = VoiceMap
  { voices   :: IntMap Sample
  , tearDown :: IntMap [Double]
  }

empty :: VoiceMap
empty = VoiceMap M.empty M.empty
{-# INLINE empty #-}

noteOn :: Pitch -> Sample -> VoiceMap -> VoiceMap
noteOn pitch sample vm = vm
  { voices = M.insert (fromIntegral pitch) sample (voices vm)
  }
{-# INLINE noteOn #-}

noteOff :: Pitch -> VoiceMap -> VoiceMap
noteOff pitch vm = vm
  { voices = M.delete pitch' (voices vm)
  , tearDown = fromMaybe (tearDown vm) $ do
      Sample _ td <- M.lookup pitch' (voices vm)
      return $ M.insert pitch' td (tearDown vm)
  }
  where
    pitch' = fromIntegral pitch
{-# INLINE noteOff #-}

size :: VoiceMap -> Int
size vm = M.size (tearDown vm) + M.size (voices vm)
{-# INLINE size #-}

interpret :: (Velocity -> Pitch -> Sample) -> MidiMessage -> VoiceMap -> VoiceMap
interpret synth (Voice _ (NoteOn pitch vel)) vm =
  noteOn pitch (synth pitch vel) vm
interpret _ (Voice _ (NoteOff pitch _)) vm =
  noteOff pitch vm
-- ingore all other messages
interpret _ _ vm = vm
{-# INLINE interpret #-}

mapAccumNotes :: (a -> Audio -> (a,Audio)) -> (a -> [Double] -> (a,[Double])) -> a -> VoiceMap -> (a, VoiceMap)
mapAccumNotes f g a0 vm =
  let (a1,voices')   = M.mapAccum (\a (Sample audio rest) -> let (a',audio') = f a audio in (a',Sample audio' rest)) a0 (voices vm)
      (a2,tearDown') = second (M.filter (not . null)) $ M.mapAccum g a1 (tearDown vm)
  in (a2,vm { voices = voices', tearDown = tearDown' })
{-# INLINE mapAccumNotes #-}
