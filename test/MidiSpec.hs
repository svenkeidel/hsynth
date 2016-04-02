{-# LANGUAGE BinaryLiterals #-}
module MidiSpec(main, spec) where

import           Control.Monad (forM_)

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import           Data.MidiFile
import           Data.MidiMessage
import           Data.Ratio
import           Data.SMPTE

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "MIDI Parser" $ do

  let noteOn pitch vel  = [0b10011010, pitch, vel]
      noteOff pitch vel = [0b10001010, pitch, vel]

  it "parses single messages" $
    getMessages (B.pack (noteOn 65 100)) `shouldBe`
      [Voice 0b1010 (NoteOn 65 100)]

  it "parses multiple messages" $
    getMessages (B.pack (noteOn 65 100 ++ noteOff 65 0)) `shouldBe`
      [ Voice 0b1010 (NoteOn 65 100)
      , Voice 0b1010 (NoteOff 65 0)
      ]

  it "handles running status bytes" $
    getMessages (B.pack [ 0x90, 0x30, 0x60
                        ,       0x31, 0x60
                        ,       0x32, 0x60
                        ,       0x30, 0x00
                        ,       0x31, 0x00
                        ,       0x32, 0x00
                        , 0x91, 0x30, 0x60
                        ,       0x31, 0x60
                        , 0x92, 0x30, 0x60
                        ,       0x31, 0x60
                        ])
      `shouldBe` [ Voice 0 (NoteOn 0x30 0x60)
                 , Voice 0 (NoteOn 0x31 0x60)
                 , Voice 0 (NoteOn 0x32 0x60)
                 , Voice 0 (NoteOff 0x30 0x00)
                 , Voice 0 (NoteOff 0x31 0x00)
                 , Voice 0 (NoteOff 0x32 0x00)
                 , Voice 1 (NoteOn 0x30 0x60)
                 , Voice 1 (NoteOn 0x31 0x60)
                 , Voice 2 (NoteOn 0x30 0x60)
                 , Voice 2 (NoteOn 0x31 0x60)
                 ]

  it "parses variable length quantities" $ do
    let tests = [ ([0b10000110, 0b11000011, 0b00010111], 106903)
                , (               [0x00], 0x00)
                , (               [0x7F], 0x7F)
                , (          [0xC0,0x00], 0x2000)
                , (          [0xFF,0x7F], 0x3FFF)
                , (     [0x81,0x80,0x00], 0x4000)
                , (     [0xFF,0xFF,0x7F], 0x1FFFFF)
                , ([0x81,0x80,0x80,0x00], 0x200000)
                , ([0xC0,0x80,0x80,0x00], 0x8000000)
                , ([0xFF,0xFF,0xFF,0x7F], 0xFFFFFFF)
                ]
    forM_ tests $ \(vlq,int) ->
      runGet getVariableLengthQuantity (B.pack vlq)
        `shouldBe` int

  it "parses SMPTE tempo information" $
    runGet getTempo (B.pack [-25,40])
      `shouldBe` SMPTE (SMPTEFormat 25 40)

  it "parses system exclusive events" $
    runGet (getTrackEvents 1)
      (B.pack [ 0x64
              , 0xF0, 0x03, 0x43, 0x12, 0x00
              , 0x81, 0x48
              , 0xF7, 0x06, 0x43, 0x12, 0x00, 0x43, 0x12, 0x00
              , 0x64
              , 0xF7, 0x04, 0x43, 0x12, 0x00, 0xF7
              ]) `shouldBe`
              [ (100,SystemExclusiveStart (B.pack [0x43, 0x12, 0x00]))
              , (200,SystemExclusiveContinue (B.pack [0x43, 0x12, 0x00, 0x43, 0x12, 0x00]))
              , (100,SystemExclusiveEnd (B.pack [0x43, 0x12, 0x00]))
              ]

  it "parses MIDI files" $ do
    let file = B.pack
         [ 0x4D, 0x54, 0x68, 0x64 -- MThd
         , 0x00, 0x00, 0x00, 0x06 -- chunk length: 6
         , 0x00, 0x00             -- format: 0
         , 0x00, 0x01             -- tracks: 1
         , 0x00, 0x60             -- 96 per quarter node
         , 0x4D, 0x54, 0x72, 0x6B -- MTrk
         , 0x00, 0x00, 0x00, 0x3D -- track length: 63 byte
         , 0x00,       0xFF, 0x58, 0x04, 0x04, 0x02, 0x18, 0x08 -- time signature
         , 0x00,       0xFF, 0x51, 0x03, 0x07, 0xA1, 0x20 -- tempo
         , 0x00,       0xC0, 0x05       -- Ch. 1 program change
         , 0x00,       0xC1, 0x2E       -- Ch. 2 program change
         , 0x00,       0xC2, 0x46       -- Ch. 3 program change
         , 0x00,       0x92, 0x30, 0x60 -- Ch. 3 note on C3, forte
         , 0x00,       0x92, 0x3C, 0x60 -- Ch. 3 note on C4, forte
         , 0x60,       0x91, 0x43, 0x40 -- Ch. 2 note on G4, mezo forte
         , 0x60,       0x90, 0x4C, 0x20 -- Ch. 1 note on E5, piano
         , 0x81, 0x40, 0x82, 0x30, 0x40 -- Ch. 3 note off C3
         , 0x00,       0x82, 0x3C, 0x40 -- Ch. 3 note off C4
         , 0x00,       0x81, 0x43, 0x40 -- Ch. 2 note off G4
         , 0x00,       0x80, 0x4C, 0x40 -- Ch. 1 note off E4
         , 0x00,       0xFF, 0x2F, 0x00 -- end of track
         ]
    runGet getMidiFile file
      `shouldBe`
      MidiFile SingleTrack (TicksPerQuarterNote 96)
        [ Track [ (0,   MetaEvent (TimeSignature (4%4) 24 8))
                , (0,   MetaEvent (SetTempo 500000))
                , (0,   MidiEvent (Voice 0 (ProgramChange 5)))
                , (0,   MidiEvent (Voice 1 (ProgramChange 46)))
                , (0,   MidiEvent (Voice 2 (ProgramChange 70)))
                , (0,   MidiEvent (Voice 2 (NoteOn 48 96)))
                , (0,   MidiEvent (Voice 2 (NoteOn 60 96)))
                , (96,  MidiEvent (Voice 1 (NoteOn 67 64)))
                , (96,  MidiEvent (Voice 0 (NoteOn 76 32)))
                , (192, MidiEvent (Voice 2 (NoteOff 48 64)))
                , (0,   MidiEvent (Voice 2 (NoteOff 60 64)))
                , (0,   MidiEvent (Voice 1 (NoteOff 67 64)))
                , (0,   MidiEvent (Voice 0 (NoteOff 76 64)))
                , (0,   MetaEvent EndOfTrack)
                ]
        ]
