{-# LANGUAGE MagicHash #-}
module Main where

import           Criterion.Main

import qualified GHC.Prim as Prim
import           GHC.Types

main :: IO ()
main =
  defaultMain
    [ bgroup "Numeric Operations"
      [ bgroup "Boxed"
        [ bgroup "Double"
          [ bench "Addition" $ nf (1.234 +) (5.678 :: Double)
          , bench "Substraction" $ nf (1.234 -) (5.678 :: Double)
          , bench "Multiplication" $ nf (1.234 *) (5.678 :: Double)
          ]
        , bgroup "Float"
          [ bench "Addition" $ nf (1.234 +) (5.678 :: Float)
          , bench "Substraction" $ nf (1.234 -) (5.678 :: Float)
          , bench "Multiplication" $ nf (1.234 *) (5.678 :: Float)
          ]
        , bgroup "Int"
          [ bench "Addition" $ nf (1 +) (5 :: Int)
          , bench "Substraction" $ nf (1 -) (5 :: Int)
          , bench "Multiplication" $ nf (1 *) (5 :: Int)
          ]
        ]
      , bgroup "Unboxed"
        [ bgroup "Double"
          [ bench "Addition" $ nf (\(D# d) -> D# (1.234## Prim.+## d)) (D# 5.678##)
          , bench "Subtraction" $ nf (\(D# d) -> D# (1.234## Prim.-## d)) (D# 5.678##)
          , bench "Multiplication" $ nf (\(D# d) -> D# (1.234## Prim.*## d)) (D# 5.678##)
          ]
        , bgroup "Float"
          [ bench "Addition" $ nf (\(F# d) -> F# (Prim.plusFloat# 1.234# d)) (F# 5.678#)
          , bench "Subtraction" $ nf (\(F# d) -> F# (Prim.minusFloat# 1.234# d)) (F# 5.678#)
          , bench "Multiplication" $ nf (\(F# d) -> F# (Prim.timesFloat# 1.234# d)) (F# 5.678#)
          ]
        , bgroup "Int"
          [ bench "Addition" $ nf (\(I# d) -> I# (1# Prim.+# d)) (I# 5#)
          , bench "Subtraction" $ nf (\(I# d) -> I# (1# Prim.-# d)) (I# 5#)
          , bench "Multiplication" $ nf (\(I# d) -> I# (1# Prim.*# d)) (I# 5#)
          ]
        ]
      ]
    , bgroup "Store Operations"
      [ 
      ]
    ]
