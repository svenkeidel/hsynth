module Sound.Oscillators.Square where

import           Prelude (($))
import           Language.Frontend
import           Sound.Types

square :: Frequency -> Rate -> Signal () Double
square freq rate =
    let halfPeriod = int $ round $ freq / fromIntegral rate
    in unfold (\(x,i) ->
                 cond (i >= halfPeriod)
                      (negate x :: Expr Double, (negate x,0 :: Expr Int))
                      (x,(x,i+1)))
           (1,0)
