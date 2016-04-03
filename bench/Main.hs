{-# LANGUAGE MagicHash #-}
module Main where

import           Criterion.Main

import qualified GHC.Prim as Prim
import           GHC.Types

main :: IO ()
main =
  defaultMain
    [ bgroup "Numeric Operations"
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
    , bgroup "Numeric Operations Unboxed"
        [ bench "addition" $ nf (\(D# d) -> D# (1.234## Prim.+## d)) (D# 5.678##)
        ]
    ]
