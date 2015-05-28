module Sound.Driver.Pulse where

import           Control.Exception (bracket)

import           Data.Stream (Stream)
import qualified Data.Stream as S

import           Sound.Quantization
import           Sound.Pulse.Simple
import           Sound.Types

runAudio :: Rate -> Stream Double -> IO ()
runAudio rate stream0 = do
  pulse <- simpleNew Nothing "hsynth" Play Nothing "haskell synthesizer" (SampleSpec (S32 LittleEndian) rate 1) Nothing Nothing
  go pulse (fmap quantizeSigned32 stream0)
  where
    go pulse stream = do
      let (chunk,rest) = S.splitAt chunkSize stream
      simpleWrite pulse chunk
      go pulse rest

    chunkSize = 1024

withPulse :: Rate -> (Simple -> IO ()) -> IO ()
withPulse rate =
  bracket (simpleNew Nothing "hsynth" Play Nothing "haskell synthesizer" (SampleSpec (S32 LittleEndian) rate 1) Nothing Nothing)
          simpleFree

writeChunk :: Simple -> [Double] -> IO ()
writeChunk pulse = simpleWrite pulse . fmap quantizeSigned32
