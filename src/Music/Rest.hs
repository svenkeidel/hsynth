module Music.Rest where

import           Data.Ratio
import           Music.Types

newtype Rest = Rest Duration

class IsRest r where
  mkRest :: Rational -> r

instance IsRest Rest where
  mkRest = Rest

doubleWholeRest, breveRest :: IsRest r => r
doubleWholeRest = mkRest 2
breveRest = doubleWholeRest
{-# INLINE doubleWholeRest #-}
{-# INLINE breveRest #-}

wholeRest, semibreveRest :: IsRest r => r
wholeRest = mkRest 1
semibreveRest = wholeRest
{-# INLINE wholeRest #-}
{-# INLINE semibreveRest #-}

halfRest, minimRest :: IsRest r => r
halfRest = mkRest (1%2)
minimRest = halfRest
{-# INLINE halfRest #-}
{-# INLINE minimRest #-}

quarterRest, crotchetRest :: IsRest r => r
quarterRest = mkRest (1%4)
crotchetRest = quarterRest
{-# INLINE quarterRest #-}
{-# INLINE crotchetRest #-}

eighthRest, quaverRest :: IsRest r => r
eighthRest = mkRest (1%8)
quaverRest = eighthRest
{-# INLINE eighthRest #-}
{-# INLINE quaverRest #-}

sixteenthRest, semiquaverRest :: IsRest r => r
sixteenthRest = mkRest (1%16)
semiquaverRest = sixteenthRest
{-# INLINE sixteenthRest #-}
{-# INLINE semiquaverRest #-}
