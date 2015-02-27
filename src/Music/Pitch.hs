module Music.Pitch where

import Sound.Types (Frequency)

newtype Pitch = Pitch Int

instance Show Pitch where
  show p@(Pitch n) =
    show (pitchClass p) ++ show (div n 12)

data PitchClass
  = Cff | Cf | C | Cs | Css
  | Dff | Df | D | Ds | Dss
  | Eff | Ef | E | Es | Ess
  | Fff | Ff | F | Fs | Fss
  | Gff | Gf | G | Gs | Gss
  | Aff | Af | A | As | Ass
  | Bff | Bf | B | Bs | Bss

instance Show PitchClass where
  show pc = case pc of
    Cff -> ff "C"; Cf -> f "C"; C -> "C"; Cs -> s "C"; Css -> ss "C"
    Dff -> ff "D"; Df -> f "D"; D -> "D"; Ds -> s "D"; Dss -> ss "D"
    Eff -> ff "E"; Ef -> f "E"; E -> "E"; Es -> s "E"; Ess -> ss "E"
    Fff -> ff "F"; Ff -> f "F"; F -> "F"; Fs -> s "F"; Fss -> ss "F"
    Gff -> ff "G"; Gf -> f "G"; G -> "G"; Gs -> s "G"; Gss -> ss "G"
    Aff -> ff "A"; Af -> f "A"; A -> "A"; As -> s "A"; Ass -> ss "A"
    Bff -> ff "B"; Bf -> f "B"; B -> "B"; Bs -> s "B"; Bss -> ss "B"
    where
      f = (++ "♭")
      ff = f . f
      s = (++ "♯")
      ss = s . s

pitchClassToInt :: PitchClass -> Int
pitchClassToInt pc = case pc of
    Cff -> -2; Cf -> -1; C ->  0; Cs ->  1; Css ->  2
    Dff ->  0; Df ->  1; D ->  2; Ds ->  3; Dss ->  4
    Eff ->  2; Ef ->  3; E ->  4; Es ->  5; Ess ->  6
    Fff ->  3; Ff ->  4; F ->  5; Fs ->  6; Fss ->  7
    Gff ->  5; Gf ->  6; G ->  7; Gs ->  8; Gss ->  9
    Aff ->  7; Af ->  8; A ->  9; As -> 10; Ass -> 11
    Bff ->  9; Bf -> 10; B -> 11; Bs -> 12; Bss -> 13 

pitchClass :: Pitch -> PitchClass
pitchClass (Pitch p) = [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] !! (p `mod` 12)
{-# INLINE pitchClass #-}

transpose :: Int -> Pitch -> Pitch
transpose i (Pitch n) = Pitch (n+i)
{-# INLINE transpose #-}

type Octave = Int
  
pitch :: PitchClass -> Octave -> Pitch
pitch pc octave = Pitch $ pitchClassToInt pc + octave * 12
{-# INLINE pitch #-}

pitchToFrequency440 :: Pitch -> Frequency
pitchToFrequency440 = pitchToFrequency 440
{-# INLINE pitchToFrequency440 #-}

type ConcertPitch = Frequency

pitchToFrequency :: ConcertPitch -> Pitch -> Frequency
pitchToFrequency concertPitch (Pitch n) =
  2 ** ((n'-57)/12) * concertPitch
  where
    n' = fromIntegral n
{-# INLINE pitchToFrequency #-}

