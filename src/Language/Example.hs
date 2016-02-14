module Language.Example where

import           Language.Expression.Typed
import qualified Language.Expression.Untyped as Untyped
import qualified Language.CodeGen as CodeGen
import           Language.SynArrow
import           Control.Category
import           Data.Sequence (Seq)

unfold :: Function c (b,c) -> Prod c -> SynArrow Prod Function d b
unfold f s = Loop (Arr (Function Proj2) >>> Arr f >>> second (Init s))

type Frequency = Double
type Rate = Int

sinA :: Frequency -> Rate -> SynArrow Prod Function d Double
sinA freq rate =
  let omh  = 2 * pi * freq / fromIntegral rate
      i    = sin omh
      c    = 2 * cos omh
  in unfold (Function $ \x -> let y1 = Proj1 x; y2 = Proj2 x; y = Const c * y1 - y2 in Inj y2 (Inj y y1)) (Prod (Constant i) (Constant 0))

example = Arr $
          Function $ \x ->
            Inj (Inj (Proj2 (Proj1 x)) (Proj1 (Proj2 x)))
                (Inj (Proj1 (Proj1 x)) (Proj2 (Proj2 x)))

optimize' :: SynArrow Prod Function b c -> (Untyped.Prod,Seq CodeGen.Assignment)
optimize' a = case optimize a of
  (Arr (Function f)) -> (Untyped.Unit, CodeGen.assignments (f Var))
  (LoopB i (Arr (Function f))) -> (Untyped.lowerProduct i, CodeGen.assignments (f Var))
  _ -> error "Optimize didn't normalize to normal form"

test = optimize (sinA 440 48000)
