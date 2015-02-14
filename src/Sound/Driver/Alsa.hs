module Sound.Driver.Alsa where

import           Control.Applicative
import           Control.Exception (bracket)

import           Data.Stream (Stream(..))

import           Foreign.Marshal.Alloc (mallocBytes,free)
import           Foreign.Storable (Storable,poke,alignment)
import           Foreign.Ptr (Ptr,alignPtr)

import           Sound.ALSA.PCM (SampleFmt)
import           Sound.ALSA.PCM.Node.ALSA (Handle,Interleaved)
import qualified Sound.ALSA.PCM.Node.ALSA as A
import qualified Sound.ALSA.PCM.Parameters.Hardware as H
import qualified Sound.ALSA.PCM.Parameters.Software as S
import           Sound.Types

bufferSize :: Int
bufferSize = 2^(13 :: Int)

withAlsa :: SampleFmt sample
         => Rate
         -> (Handle Interleaved sample -> IO ())
         -> IO ()
withAlsa rate = bracket open close
  where
    open = snd <$> A.open (A.modes []) A.StreamPlayback hwParam swParam "default"
    close handle = do
      A.drain handle
      A.close handle
    hwParam = do
      H.setRateResample False
      rate' <- fst <$> H.setRateNear rate EQ
      size' <- fst <$> H.setBufferSizeNear bufferSize
      return (rate',size')
    swParam q@(sizes,_,_) = do
      setBufferSize sizes
      return q

runAudio :: Rate -> Audio -> IO ()
runAudio rate audio =
  withAlsa rate $ \handle ->
  bracket (mallocBytes bufferSize) free $ \ptr -> do
    audio' <- store bufferSize audio ptr
    A.writeiRetry handle ptr bufferSize
    return ()

store :: Storable a => Int -> Stream a -> Ptr a -> IO (Stream a)
store 0 stream _        = return stream
store n (Cons x xs) ptr = do
  poke ptr x
  store (n-1) xs (alignPtr ptr (alignment x))
