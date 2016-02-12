module Language.Signal where

import Control.Arrow

class ArrowInit a where
  init :: b -> a b b

class (Arrow a, ArrowInit a, ArrowLoop a) => Signal a
