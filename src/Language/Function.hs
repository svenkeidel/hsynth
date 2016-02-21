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
  Sin :: Fun (Double -> Double)
  Cos :: Fun (Double -> Double)

deriving instance Show (Fun a)

