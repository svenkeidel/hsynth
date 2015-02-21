module Sound.Driver.StdOut where

import           Prelude hiding (concat)

import           Data.Monoid (mappend)
import           Data.Stream (Stream(..))
import           Data.ByteString.Lazy.Builder

import           Sound.Quantization

import           System.IO (Handle,stdout)

type ChunkSize = Int

{-runAudio8 :: Rate -> Stream Double -> IO ()-}
{-runAudio8 rate stream = hRunAudio stdout 1024 rate (fmap quantizeUnsigned8 stream)-}
{-[># INLINE runAudio8 #<]-}

runAudio16 :: Stream Double -> IO ()
runAudio16 = hRunAudio stdout . fmap (int16LE . quantizeSigned16)
{-# INLINE runAudio16 #-}

{-runAudio32 :: Rate -> Stream Double -> IO ()-}
{-runAudio32 rate stream = hRunAudio stdout 1024 rate (fmap quantizeSigned32 stream)-}

hRunAudio :: Handle -> Stream Builder -> IO ()
hRunAudio handle stream = hPutBuilder handle (concat stream)

concat :: Stream Builder -> Builder
concat (Cons a as) = a `mappend` (concat as)

{-
hRunAudio :: Storable a => Handle -> ChunkSize -> Rate -> Stream a -> IO ()
hRunAudio handle chunkSize _ stream =
  bracket (mallocArray chunkSize) free $ \buffer ->
    writeStream chunkSize handle buffer stream
{-# INLINE hRunAudio #-}

writeStream :: Storable a => ChunkSize -> Handle -> Ptr a -> Stream a -> IO ()
writeStream chunkSize handle ptr stream = do
  stream' <- pokeStream chunkSize ptr stream
  _       <- hPutBuf handle ptr chunkSize
  writeStream chunkSize handle ptr stream'

pokeStream :: Storable a => Int -> Ptr a -> Stream a -> IO (Stream a)
pokeStream 0 _ stream = return stream
pokeStream n ptr (Cons a as) = do
  poke ptr a
  pokeStream (n-1) (advancePtr ptr 1) as

{-hRunAudio16 :: Handle -> Duration -> Rate -> Stream Double -> IO ()-}
{-hRunAudio16 handle dur rate stream =-}
  {-B.hPutStr handle-}
    {-$ B.pack-}
    {-$ S.take numberOfSamples-}
    {-$ fmap discretize16 stream-}
  {-where-}
    {-numberOfSamples = truncate (dur * fromIntegral rate)-}
-}
