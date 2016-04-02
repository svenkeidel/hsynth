{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Function where

data Fun a where
  Add :: Fun (Double -> Double -> Double)
  Mult :: Fun (Double -> Double -> Double)
  Sub :: Fun (Double -> Double -> Double)
  Div :: Fun (Double -> Double -> Double)

  Abs :: Fun (Double -> Double)
  Signum :: Fun (Double -> Double)

  Exp :: Fun (Double -> Double)
  Log :: Fun (Double -> Double)
  Sqrt :: Fun (Double -> Double)
  LogBase :: Fun (Double -> Double)
  Pow :: Fun (Double -> Double -> Double)

  Sin :: Fun (Double -> Double)
  Asin :: Fun (Double -> Double)
  Sinh :: Fun (Double -> Double)
  Asinh :: Fun (Double -> Double)

  Cos :: Fun (Double -> Double)
  Acos :: Fun (Double -> Double)
  Cosh :: Fun (Double -> Double)
  Acosh :: Fun (Double -> Double)

  Tan :: Fun (Double -> Double)
  Atan :: Fun (Double -> Double)
  Tanh :: Fun (Double -> Double)
  Atanh :: Fun (Double -> Double)



deriving instance Show (Fun a)
