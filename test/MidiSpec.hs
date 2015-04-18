{-# LANGUAGE QuasiQuotes #-}
module MidiSpec(main, spec) where

import qualified Data.ByteString.Lazy as B
import           Language.Literals.Binary (b)
import           Music.Midi
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "the MIDI Protocol" $ do

  let noteOn pitch vel = [[b| 1001 1010 |], pitch, vel]
      noteOff pitch vel = [[b| 1000 1010 |], pitch, vel]

  it "has messages when a key on the piano is pressed" $
    getMessages (B.pack (noteOn 65 100)) `shouldBe`
      [Voice [b| 1010 |] (NoteOn 65 100)]

  it "can parse multiple messages" $
    getMessages (B.pack (noteOn 65 100 ++ noteOff 65 0)) `shouldBe`
      [ Voice [b| 1010 |] (NoteOn 65 100)
      , Voice [b| 1010 |] (NoteOff 65 0)
      ]
