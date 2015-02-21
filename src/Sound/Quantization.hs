{-# LANGUAGE ScopedTypeVariables #-}
module Sound.Quantization where

import Data.Word
import Data.Int

quantizeUnsigned8 :: Double -> Word8
quantizeUnsigned8 = quantizeUnsigned
{-# INLINE quantizeUnsigned8 #-}

quantizeUnsigned16 :: Double -> Word16
quantizeUnsigned16 = quantizeUnsigned
{-# INLINE quantizeUnsigned16 #-}

quantizeUnsigned32 :: Double -> Word32
quantizeUnsigned32 = quantizeUnsigned
{-# INLINE quantizeUnsigned32 #-}

quantizeSigned8 :: Double -> Int8
quantizeSigned8 = quantizeSigned
{-# INLINE quantizeSigned8 #-}

quantizeSigned16 :: Double -> Int16
quantizeSigned16 = quantizeSigned
{-# INLINE quantizeSigned16 #-}

quantizeSigned32 :: Double -> Int32
quantizeSigned32 = quantizeSigned
{-# INLINE quantizeSigned32 #-}

quantizeSigned :: forall a. (Integral a, Bounded a) => Double -> a
quantizeSigned x = round $ x * fromIntegral (maxBound :: a)
{-# INLINE quantizeSigned #-}

quantizeUnsigned :: forall a. (Integral a, Bounded a) => Double -> a
quantizeUnsigned x = round $ (x+1) * fromIntegral (maxBound :: a) / 2
{-# INLINE quantizeUnsigned #-}
