{-# LANGUAGE TemplateHaskell #-}
module Language.Example where

import           Prelude hiding (id)
import           Language.Expression
import           Language.SimpleExpression (SimpleExpr)
import qualified Language.SimpleExpression as Simple
import           Language.CodeGen.TH
import           Language.SynArrow
--import           Language.Compile
import           Control.Category
import           Data.Sequence (Seq)

unfold :: Function c (b,c) -> SimpleExpr c -> SynArrow SimpleExpr Function d b
unfold (Function f) s = LoopD s (Function $ \x -> f (Proj2 x))

type Frequency = Double
type Rate = Int

sinA :: Frequency -> Rate -> SynArrow SimpleExpr Function d Double
sinA freq rate =
  let omh  = 2 * pi * freq / fromIntegral rate
      i    = sin omh
      c    = 2 * cos omh
  in unfold (Function $ \x -> let y1 = Proj1 x; y2 = Proj2 x; y = Const c * y1 - y2 in Inj y2 (Inj y y1))
                                       (inj (Simple.Const i) (Simple.Const 0))

--test = $(compile (sinA 440 48000))
