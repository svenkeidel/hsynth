{-# LANGUAGE BinaryLiterals #-}
module MidiSpec(main, spec) where

import qualified Data.ByteString.Lazy as B
import           Music.Midi
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "the MIDI Protocol" $ do

  let noteOn pitch vel  = [0b10011010, pitch, vel]
      noteOff pitch vel = [0b10001010, pitch, vel]

  it "has messages when a key on the piano is pressed" $
    getMessages (B.pack (noteOn 65 100)) `shouldBe`
      [Voice 0b1010 (NoteOn 65 100)]

  it "can parse multiple messages" $
    getMessages (B.pack (noteOn 65 100 ++ noteOff 65 0)) `shouldBe`
      [ Voice 0b1010 (NoteOn 65 100)
      , Voice 0b1010 (NoteOff 65 0)
      ]
