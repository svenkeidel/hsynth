module Main where

{-import           Control.Monad.Primitive-}
import           Criterion.Main

import qualified Data.Stream as S
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Primitive as VP
import qualified Data.List as L

import           Music.ConcertPitch (a440)
import           Music.Pitch
import           Music.VoiceMap (VoiceMap)
import qualified Music.VoiceMap as VM
import           Music.Tuning.EqualTemperament

import           Sound.Sine
import           Sound.Types
import           Sound.Flute
import           Sound.Sample
import           Sound.AllPassFilter

import qualified System.Random as R

main :: IO ()
main = do
  g <- R.newStdGen
  let audio freq rate = flute 5 0.5 freq 0.9 0.02 g rate
      sig p rate = Sample (audio (tuning p) rate) (const [])
      sampleRate = 48000
      vm = VM.noteOn 64 (sig 64 sampleRate)
         $ VM.noteOn 65 (sig 65 sampleRate)
         $ VM.noteOn 66 (sig 66 sampleRate)
         $ VM.empty
      benchSize = 1024

  defaultMain
    [ bgroup "VoiceMap"
      [ bench "single values/Vector" $ nf (getVector benchSize) vm
      , bench "single values/List" $ nf (getList benchSize) vm
      , bench "batch/Vector"  $ nf (fst . batchVector benchSize) vm
      , bench "batch/Vector/Storable"  $ nf (fst . batchVectorStorable benchSize) vm
      , bench "batch/Vector/Primitive"  $ nf (fst . batchVectorPrimitive benchSize) vm
      , bench "batch/List"  $ nf (fst . batchList benchSize) vm
      ]
    , let n = 48000 * 5
      in bgroup "AllPassFilter"
      [ bench "100" $ nf (allPassFilter 100 0.5 (sinA 440 48000) S.!!) n
      , bench "1000" $ nf (allPassFilter 1000 0.5 (sinA 440 48000) S.!!) n
      , bench "10000" $ nf (allPassFilter 10000 0.5 (sinA 440 48000) S.!!) n
      , bench "control" $ nf (sinA 440 48000 S.!!) n
      ]
    ]

  where
    tuning = equalTemperament twelveTET a440 . fromMidiPitch

    --

    singleValue :: VoiceMap -> (Double,VoiceMap)
    singleValue vm =
      let (d,vm') = VM.mapAccumNotes (\amp (S.Cons x xs) -> (amp+x,xs)) (curry id) 0 vm
      in (d / fromIntegral (VM.size vm), vm')

    getVector n = V.unfoldrN n (Just . singleValue)
    getList n = L.take n . L.unfoldr (Just . singleValue)

    --

    batchVector :: Int -> VoiceMap -> (Vector Double,VoiceMap)
    batchVector n vm =
      VM.mapAccumNotes (mixSampleVector n) (curry id) (V.replicate n 0) vm

    mixSampleVector :: Int -> Vector Double -> Audio -> (Vector Double, Audio)
    mixSampleVector n sample audio = 
      let (sample',audio') = S.splitAt (fromIntegral n) audio
      in (V.zipWith (+) sample (V.fromList sample'),audio')

    --

    batchList :: Int -> VoiceMap -> ([Double],VoiceMap)
    batchList n vm =
      VM.mapAccumNotes (mixSampleList n) (curry id) (replicate n 0) vm

    mixSampleList :: Int -> [Double] -> Audio -> ([Double], Audio)
    mixSampleList n sample audio = 
      let (sample',audio') = S.splitAt (fromIntegral n) audio
      in (zipWith (+) sample sample',audio')

    --

    batchVectorStorable :: Int -> VoiceMap -> (VS.Vector Double,VoiceMap)
    batchVectorStorable n vm =
      VM.mapAccumNotes (mixSampleStorable n) (curry id) (VS.replicate n 0) vm

    mixSampleStorable :: Int -> VS.Vector Double -> Audio -> (VS.Vector Double, Audio)
    mixSampleStorable n sample audio = 
      let (sample',audio') = S.splitAt (fromIntegral n) audio
      in (VS.zipWith (+) sample (VS.fromList sample'),audio')

    --

    batchVectorPrimitive :: Int -> VoiceMap -> (VP.Vector Double,VoiceMap)
    batchVectorPrimitive n vm =
      VM.mapAccumNotes (mixSampleVectorPrimitive n) (curry id) (VP.replicate n 0) vm

    mixSampleVectorPrimitive :: Int -> VP.Vector Double -> Audio -> (VP.Vector Double, Audio)
    mixSampleVectorPrimitive n sample audio = 
      let (sample',audio') = S.splitAt (fromIntegral n) audio
      in (VP.zipWith (+) sample (VP.fromList sample'),audio')
