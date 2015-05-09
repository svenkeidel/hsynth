{-# LANGUAGE DeriveFunctor #-}
module Music.Tuning.Pythagorean where

import           Control.Arrow ((>>>))

import           Music.Interval
import           Music.JustInterval
import           Music.Pitch
import           Music.ConcertPitch (ConcertPitch(..))
import           Sound.Types

data PythagoreanTuning =
  PythagoreanTuning
  { generator :: Interval
  , order :: PitchClass -> Ordinal
  }

threeLimit :: PythagoreanTuning
threeLimit =
  PythagoreanTuning
  { generator = perfectFifth
  , order = vertical >>> reorder 9 >>> shift 14
  }

pythagoreanInterval :: PythagoreanTuning -> PitchClass -> Interval
pythagoreanInterval tuning pc =
  let i = order tuning pc
      i' = fromIntegral i :: Double
      k = if i < 0 then truncate ((-i'+2) / 2) else negate (truncate (i'/2)) :: Int
      g = ratio (generator tuning)
  in Interval (g^^i * 2^^k)

pythagorean :: PythagoreanTuning -> ConcertPitch -> Pitch -> Frequency
pythagorean tuning (ConcertPitch (Pitch cpc cpOct) cpFreq) (Pitch pc octave) =
  let (Interval i) = pythagoreanInterval tuning pc
  in 2^^octave * c0 * fromRational i
  where
    c0 :: Frequency
    c0 = cpFreq / (realToFrac (pythagoreanInterval tuning cpc) * 2^^(cpOct::Int))

notes :: Table PitchClass
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
  foldMap f (Table _ tab) = mconcat $ map (foldMap f) tab
  foldr f z (Table _ tab) = go z tab
    where
      go b0 [] = b0
      go b0 ([]:ys) = go b0 ys
      go b0 ((x:xs):ys) = f x (go b0 (xs:ys))

instance Show a => Show (Table a) where
  show tab@(Table aln _) = unlines (map unwords (tabData (fmap align showed)))
    where
      showed  = fmap (\s -> let sw = show s in (length sw, sw)) tab
      m = fst $ maximum showed
      align (l,s) = case aln of
        LeftAlign  -> s ++ replicate (m - l) ' '
        RightAlign -> replicate (m - l) ' ' ++ s
