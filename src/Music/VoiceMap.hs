module Music.VoiceMap
( VoiceMap
, empty
, noteOn
, noteOff
, mapAccumNotes
, interpret
, roll
, size
) where

import           Data.Maybe (fromMaybe)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as M
import qualified Data.List as L
import qualified Data.Stream as S
import qualified Data.Vector.Storable as V
import           Data.Vector.Storable (Vector)

import           Music.Midi (Pitch,Velocity,MidiMessage(..),ChannelVoiceMessage(..))

import           Sound.Sample
import           Sound.Types

-- | Maintains audio signals of a specific MIDI pitch
data VoiceMap = VoiceMap
  { voices   :: IntMap Sample
  , tearDown :: [Double]
  }

empty :: VoiceMap
empty = VoiceMap M.empty []
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
      Sample audio td <- M.lookup pitch' (voices vm)
      return $ zipWith (+) (td audio) (tearDown vm)
  }
  where
    pitch' = fromIntegral pitch
{-# INLINE noteOff #-}

size :: VoiceMap -> Int
size vm = M.size (voices vm) + length (tearDown vm)
{-# INLINE size #-}

interpret :: (Velocity -> Pitch -> Sample) -> MidiMessage -> VoiceMap -> VoiceMap
interpret synth (Voice _ (NoteOn pitch vel)) vm =
  noteOn pitch (synth vel pitch) vm
interpret _ (Voice _ (NoteOff pitch _)) vm =
  noteOff pitch vm
-- ingore all other messages
interpret _ _ vm = vm
{-# INLINE interpret #-}

mapAccumNotes :: (a -> Audio -> (a,Audio)) -> (a -> [Double] -> (a,[Double])) -> a -> VoiceMap -> (a, VoiceMap)
mapAccumNotes f g a0 vm =
  let (a1,voices')   = M.mapAccum (\a (Sample audio rest) -> let (a',audio') = f a audio in (a',Sample audio' rest)) a0 (voices vm)
      (a2,tearDown') = g a1 (tearDown vm)
  in (a2,vm { voices = voices', tearDown = tearDown' })
{-# INLINE mapAccumNotes #-}

{-roll :: VoiceMap -> (Double,VoiceMap)-}
{-roll vm = mapAccumNotes goAudio goTearDown 0 vm-}
  {-where-}
    {-goAudio amp (S.Cons x xs) = (amp+x,xs)-}
    {-goTearDown amp (x:xs) = (amp+x,xs)-}
    {-goTearDown amp []     = (amp,[])-}

roll :: Int -> VoiceMap -> (Vector Double,VoiceMap)
roll n = mapAccumNotes mixAudio mixTearDown (V.replicate n 0)
  where
    mixAudio :: Vector Double -> Audio -> (Vector Double, Audio)
    mixAudio sample audio =
      let (sample',audio') = S.splitAt n audio
      in (V.zipWith (+) sample (V.fromList sample'),audio')
    mixTearDown :: Vector Double -> [Double] -> (Vector Double,[Double])
    mixTearDown sample audio =
      let (sample',audio') = L.splitAt n audio
      in (V.zipWith (+) sample (V.fromList sample'),audio')
