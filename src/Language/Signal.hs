{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Signal where

import Control.Arrow

class ArrowInit i a | a -> i where
  init :: i b -> a b b

class (Arrow a, ArrowInit i a, ArrowLoop a) => Signal i a | a -> i
