{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Example where

import qualified Prelude as P
import           Language.Frontend

unfold :: (Syntax (Expr c), Syntax (Expr b), Syntax (Expr d))
       => Fun (Expr c) (Expr (b,c)) -> SimpleExpr c -> Signal d b
unfold f s = loop (arr (\(_,x) -> f x) >>> second (init s))

type Frequency = Double
type Rate = Int

sinA :: Frequency -> Rate -> Signal () Double
sinA freq rate =
  let omh  = 2 P.* P.pi P.* freq P./ P.fromIntegral rate
      i    = P.sin omh
      c    = 2 P.* P.cos omh
  in unfold (\(y1,y2) -> let y = double c * y1 - y2 in (y2,(y,y1)))
            (externalize (i,0) :: SimpleExpr (Double,Double))
