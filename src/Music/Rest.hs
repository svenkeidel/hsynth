module Music.Rest where

import           Data.Ratio
import           Music.Types

newtype Rest = Rest Duration

doubleWholeRest, breveRest :: Rest
doubleWholeRest = Rest 2
breveRest = doubleWholeRest
{-# INLINE doubleWholeRest #-}
{-# INLINE breveRest #-}

wholeRest, semibreveRest :: Rest
wholeRest = Rest 1
semibreveRest = wholeRest
{-# INLINE wholeRest #-}
{-# INLINE semibreveRest #-}

halfRest, minimRest :: Rest
halfRest = Rest (1%2)
minimRest = halfRest
{-# INLINE halfRest #-}
{-# INLINE minimRest #-}

quarterRest, crotchetRest :: Rest
quarterRest = Rest (1%4)
crotchetRest = quarterRest
{-# INLINE quarterRest #-}
{-# INLINE crotchetRest #-}

eighthRest, quaverRest :: Rest
eighthRest = Rest (1%8)
quaverRest = eighthRest
{-# INLINE eighthRest #-}
{-# INLINE quaverRest #-}

sixteenthRest, semiquaverRest :: Rest
sixteenthRest = Rest (1%16)
semiquaverRest = sixteenthRest
{-# INLINE sixteenthRest #-}
{-# INLINE semiquaverRest #-}
