module Data.Stream.IO where

import           Prelude hiding (lines)

import           Control.Applicative

import           Data.Stream (Stream(..))
{-import qualified Data.Stream as S-}

import           System.IO.Unsafe

lines :: IO (Stream String)
lines = unsafeStreamIO getLine

unsafeStreamIO :: IO a -> IO (Stream a)
unsafeStreamIO io = unsafeInterleaveIO $ do
  a <- io
  Cons a <$> unsafeStreamIO io
