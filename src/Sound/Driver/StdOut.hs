module Sound.Driver.StdOut where

import qualified Data.ByteString as B
import qualified Data.Stream as S
import           Data.Stream (Stream)

import           Sound.Discretization
import           Sound.Types

type ChunkSize = Int

runAudio :: Duration -> Rate -> Stream Double -> IO ()
runAudio dur rate stream =
  B.putStr $ B.pack
           $ S.take numberOfSamples
           $ fmap discretize8 stream
  where
    numberOfSamples = truncate (dur * fromIntegral rate)
