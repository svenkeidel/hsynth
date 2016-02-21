{-# LANGUAGE TemplateHaskell #-}
module Language.Example where

import           Prelude hiding (id)
import           Language.Product
import           Language.Expression
import           Language.SimpleExpression (SimpleExpr)
import qualified Language.SimpleExpression as Simple
import           Language.CodeGen.TH
import           Language.SynArrow
--import           Language.Compile
import           Control.Category
import           Data.Sequence (Seq)

unfold :: Function c (b,c) -> SimpleExpr c -> SynArrow SimpleExpr Function d b
unfold f s = LoopD s (Arr (Function Proj2) >>> Arr f)

type Frequency = Double
type Rate = Int

sinA :: Frequency -> Rate -> SynArrow SimpleExpr Function d Double
sinA freq rate =
  let omh  = 2 * pi * freq / fromIntegral rate
      i    = sin omh
      c    = 2 * cos omh
  in unfold (Function $ \x -> let y1 = Proj1 x; y2 = Proj2 x; y = Const c * y1 - y2 in Inj y2 (Inj y y1))
                                       (inj (Simple.Const i) (Simple.Const 0))

example = Arr $
          Function $ \x ->
            Inj (Inj (Proj2 (Proj1 x)) (Proj1 (Proj2 x)))
                (Inj (Proj1 (Proj1 x)) (Proj2 (Proj2 x)))

test = $(compile (sinA 440 48000))
