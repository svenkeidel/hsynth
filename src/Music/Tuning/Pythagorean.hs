{-# LANGUAGE DeriveFunctor #-}
module Music.Tuning.Pythagorean where

import           Control.Arrow ((>>>))

import           Data.Monoid
import           Data.Foldable (Foldable)
import qualified Data.Foldable as F
import           Music.Interval
import           Music.Pitch

type Generator = Interval

pythagorean :: Generator -> PitchClass -> Interval
pythagorean gen pc =
  let i = ord pc
      i' = fromIntegral i :: Double
      k = if i < 0 then truncate ((-i'+2) / 2) else negate (truncate (i'/2)) :: Int
      g = ratio gen
  in Interval (g^^i * 2^^k)

ord :: PitchClass -> Ordinal
-- reorder 9: reorder that perfect fifth have adjacent oridnal numbers
-- shift 16: shift oridnal numbers s.t. D has ordinal 0
ord = vertical >>> reorder 9 >>> shift 16

notes = Table LeftAlign
  [ [ Cff, Cf, C, Cs, Css ]
  , [ Dff, Df, D, Ds, Dss ]
  , [ Eff, Ef, E, Es, Ess ]
  , [ Fff, Ff, F, Fs, Fss ]
  , [ Gff, Gf, G, Gs, Gss ]
  , [ Aff, Af, A, As, Ass ]
  , [ Bff, Bf, B, Bs, Bss ] ]

data Alignment = LeftAlign | RightAlign

data Table a = Table Alignment [[a]]
  deriving (Functor)

tabData :: Table a -> [[a]]
tabData (Table _ t) = t

rightAlign :: Table a -> Table a
rightAlign (Table _ tab) = (Table RightAlign tab)

instance Foldable Table where
  foldMap f (Table _ tab) = mconcat $ map (F.foldMap f) tab
  foldr f z (Table _ tab) = go z tab
    where
      go b0 [] = b0
      go b0 ([]:ys) = go b0 ys
      go b0 ((x:xs):ys) = f x (go b0 (xs:ys))

instance Show a => Show (Table a) where
  show tab@(Table aln _) = unlines (map unwords (tabData (fmap align showed)))
    where
      showed  = fmap (\s -> let sw = show s in (length sw, sw)) tab
      m = fst $ F.maximum showed
      align (l,s) = case aln of
        LeftAlign  -> s ++ replicate (m - l) ' '
        RightAlign -> replicate (m - l) ' ' ++ s
