{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Example where

import qualified Prelude as P
import           Language.Frontend

unfold :: (Syntax (Expr a c), Syntax (Expr a b), Syntax (Expr a d), Syntax (Expr a c -> Expr a (b,c)))
       => Fun (Expr a c) (Expr a (b,c)) -> SimpleExpr c -> Signal a d b
unfold f s = loop (arr (\(_,x) -> f x) >>> second (init s))

type Frequency = Double
type Rate = Int

sinA :: Frequency -> Rate -> Signal a () Double
sinA freq rate =
  let omh  = 2 * P.pi * freq P./ P.fromIntegral rate
      i    = P.sin omh
      c    = 2 * P.cos omh
  in unfold (\(y1,y2) -> let y = const c * y1 - y2 in (y2,(y,y1)))
            (sconst (i,0 :: Double))
