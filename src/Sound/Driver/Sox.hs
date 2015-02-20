module Sound.Driver.Sox where

import           Data.Monoid ((<>))
import           Data.Stream (Stream)

import           Sound.Discretization

import qualified Sound.Driver.StdOut as StdOut
import qualified Sound.Sox.Play as Sox
import qualified Sound.Sox.Format as Sox
import qualified Sound.Sox.Option.Format as Sox
import           Sound.Types

runAudio :: Rate -> Stream Double -> IO ()
runAudio rate stream = do
  _ <- Sox.simple (\handle stream' -> StdOut.hRunAudio handle 1024 rate stream') format rate
    (fmap discretize8 stream)
  return ()
  where
    format = Sox.numberOfChannels 1
          <> Sox.format Sox.unsignedByte
