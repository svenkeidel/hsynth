{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Music.Pitch where

import qualified Music.Midi as Midi

type Octave = Int

-- An alternative representation for equal temperament is `newtype Pitch = Pitch Int`.
-- Although this representation does not allow to represent commas.
-- See: http://en.wikipedia.org/wiki/Comma_(music)
data Pitch = Pitch PitchClass Octave

pitch :: PitchClass -> Octave -> Pitch
pitch = Pitch

instance Show Pitch where
  show (Pitch pitchClass octave) =
    show pitchClass ++ show octave

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

type Ordinal = Int

sharp :: Ordinal -> PitchClass
sharp p = [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] !! (p `mod` 12)
{-# INLINE sharp #-}

flat :: Ordinal -> PitchClass
flat p = [C,Df,D,Ef,E,F,Gf,G,Af,A,Bf,B] !! (p `mod` 12)
{-# INLINE flat #-}

fromMidiPitch :: Midi.Pitch -> Pitch
fromMidiPitch midiPitch =
  let mp = fromIntegral midiPitch
  in pitch (sharp mp) (mp `div` 12)

type Semitones = Int

reorder :: Semitones -> Ordinal -> Ordinal
reorder semitones o = (o * semitones) `mod` 7 + 7 * (o `div` 7)

shift :: Int -> Ordinal -> Ordinal
shift n o = o - n

vertical :: PitchClass -> Ordinal
vertical pc = case pc of
    Cff ->  0; Cf ->  7; C -> 14; Cs -> 21; Css -> 28
    Dff ->  1; Df ->  8; D -> 15; Ds -> 22; Dss -> 29
    Eff ->  2; Ef ->  9; E -> 16; Es -> 23; Ess -> 30
    Fff -> -4; Ff ->  3; F -> 10; Fs -> 17; Fss -> 24
    Gff ->  4; Gf -> 11; G -> 18; Gs -> 25; Gss -> 32
    Aff ->  5; Af -> 12; A -> 19; As -> 26; Ass -> 33
    Bff ->  6; Bf -> 13; B -> 20; Bs -> 27; Bss -> 34

horizontal :: PitchClass -> Int
horizontal pc = case pc of
    Cff -> -2; Cf -> -1; C ->  0; Cs ->  1; Css ->  2
    Dff ->  0; Df ->  1; D ->  2; Ds ->  3; Dss ->  4
    Eff ->  2; Ef ->  3; E ->  4; Es ->  5; Ess ->  6
    Fff ->  3; Ff ->  4; F ->  5; Fs ->  6; Fss ->  7
    Gff ->  5; Gf ->  6; G ->  7; Gs ->  8; Gss ->  9
    Aff ->  7; Af ->  8; A ->  9; As -> 10; Ass -> 11
    Bff ->  9; Bf -> 10; B -> 11; Bs -> 12; Bss -> 13 

-- 
-- transpose :: Int -> Pitch -> Pitch
-- transpose i (Pitch n) = Pitch (n+i)
-- {-# INLINE transpose #-}
-- 
-- type Octave = Int
--   
-- pitch :: PitchClass -> Octave -> Pitch
-- pitch pc octave = Pitch $ pitchClassToInt pc + octave * 12
-- {-# INLINE pitch #-}

-- pitchToFrequency440 :: Pitch -> Frequency
-- pitchToFrequency440 = pitchToFrequency 440
-- {-# INLINE pitchToFrequency440 #-}
-- 
-- type ConcertPitch = Frequency
-- 
-- pitchToFrequency :: ConcertPitch -> Pitch -> Frequency
-- pitchToFrequency concertPitch (Pitch n) =
--   2 ** ((n'-57)/12) * concertPitch
--   where
--     n' = fromIntegral n
-- {-# INLINE pitchToFrequency #-}

