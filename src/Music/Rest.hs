module Music.Rest where

import           Data.Ratio
import           Music.Types

newtype Rest = Rest Duration

doubleWholeRest, breveRest :: Rest
doubleWholeRest = Rest 2
breveRest = doubleWholeRest

wholeRest, semibreveRest :: Rest
wholeRest = Rest 1
semibreveRest = wholeRest

halfRest, minimRest :: Rest
halfRest = Rest (1%2)
minimRest = halfRest

quarterRest, crotchetRest :: Rest
quarterRest = Rest (1%4)
crotchetRest = quarterRest

eighthRest, quaverRest :: Rest
eighthRest = Rest (1%8)
quaverRest = eighthRest

sixteenthRest, semiquaverRest :: Rest
sixteenthRest = Rest (1%16)
semiquaverRest = sixteenthRest
