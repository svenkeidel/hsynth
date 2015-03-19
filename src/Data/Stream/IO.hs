module Data.Stream.IO where

import           Prelude hiding (lines)

import           Control.Applicative

-- import qualified Data.Char as C
import           Data.Stream (Stream(..))
-- import qualified Data.Stream as S
-- import           Music.Pitch

import           System.IO.Unsafe

lines :: IO (Stream String)
lines = unsafeStreamIO getLine

chars :: IO (Stream Char)
chars = unsafeStreamIO getChar

-- keyboard :: IO (Stream Pitch)
-- keyboard = S.mapMaybe charToPitch <$> chars

-- charToPitch :: Char -> Maybe Pitch
-- charToPitch c | C.isUpper c = transpose 12 <$> charToPitch (C.toLower c)
-- charToPitch c = case c of
--   'z' -> Just (Pitch 0)
--   's' -> Just (Pitch 1)
--   'x' -> Just (Pitch 2)
--   'd' -> Just (Pitch 3)
--   'c' -> Just (Pitch 4)
--   'v' -> Just (Pitch 5)
--   'g' -> Just (Pitch 6)
--   'b' -> Just (Pitch 7)
--   'h' -> Just (Pitch 8)
--   'n' -> Just (Pitch 9)
--   'j' -> Just (Pitch 10)
--   'm' -> Just (Pitch 11)
--   _   -> Nothing

unsafeStreamIO :: IO a -> IO (Stream a)
unsafeStreamIO io = unsafeInterleaveIO $ do
  a <- io
  Cons a <$> unsafeStreamIO io
