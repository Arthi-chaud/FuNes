module APU.PulseSpec (spec) where

import Nes.APU.State.BitField
import Nes.APU.State.Channel
import Nes.APU.State.Pulse
import Test.Hspec

spec :: Spec
spec = do
    describe "Pulse BitField" $ do
        describe "Duty" $ do
            testGet duty (mkPulse 0b10111111 0 0 0) 0b10
            testSet duty 0b10 (mkPulse 0b11111111 0 0 0) (mkPulse 0b10111111 0 0 0)

        describe "Loops" $ do
            testGet loops (mkPulse 0b00100000 0 0 0) True
            testSet loops False (mkPulse 0b11111111 0 0 0) (mkPulse 0b11011111 0 0 0)

        describe "Volume is constant" $ do
            testGet volumeIsConst (mkPulse 0b00010000 0 0 0) True
            testSet volumeIsConst False (mkPulse 0b11111111 0 0 0) (mkPulse 0b11101111 0 0 0)

        describe "Volume" $ do
            testGet volume (mkPulse 0b00000100 0 0 0) 0b100
            testSet volume 0b100 (mkPulse 0b11111111 0 0 0) (mkPulse 0b11110100 0 0 0)

        describe "Sweep" $ do
            describe "Enabled" $ do
                testGet sweepEnabled (mkPulse 0 0b10000000 0 0) True
                testSet sweepEnabled False (mkPulse 0 0b11111111 0 0) (mkPulse 0 0b01111111 0 0)

            describe "Period" $ do
                testGet sweepPeriod (mkPulse 0 0b01110000 0 0) 0b111
                testSet sweepPeriod 0 (mkPulse 0 0b11111111 0 0) (mkPulse 0 0b10001111 0 0)

            describe "Negate" $ do
                testGet sweepNegate (mkPulse 0 0b00001000 0 0) True
                testSet sweepNegate False (mkPulse 0 0b11111111 0 0) (mkPulse 0 0b11110111 0 0)

            describe "Shift" $ do
                testGet sweepShift (mkPulse 0 0b00000111 0 0) 0b111
                testSet sweepShift 0 (mkPulse 0 0b11111111 0 0) (mkPulse 0 0b11111000 0 0)

        describe "Timer" $ do
            testGet timer (mkPulse 0 0 0b00001111 0b00000111) 0b11100001111
            testSet timer 0b101 (mkPulse 0 0 0b11111111 0b11111111) (mkPulse 0 0 0b101 0b11111000)

        describe "Length Counter Load" $ do
            testGet lengthCounterLoad (mkPulse 0 0 0 0b10001111) 0b10001
            testSet lengthCounterLoad 0b101 (mkPulse 0 0 0 0b11111111) (mkPulse 0 0 0 0b00101111)
  where
    testGet getter pulse expected = it "Get" $ get getter pulse `shouldBe` expected
    testSet field newval pulse expected = it "Set" $ set field newval pulse `shouldBe` expected
    mkPulse b0 b1 b2 b3 = MkPulse $ MkChannel b0 b1 b2 b3
