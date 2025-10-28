{-# LANGUAGE TypeApplications #-}

module APU.ChannelSpec (spec) where

import Nes.APU.State.BitField
import Nes.APU.State.Channel
import Nes.APU.State.Pulse
import Nes.APU.State.Triangle (linearCounterControl, linearCounterLoad)
import Test.Hspec

spec :: Spec
spec = do
    describe "Pulse" $ do
        describe "Duty" $ do
            testGet duty (mkChannel 0b10111111 0 0 0) 0b10
            testSet duty 0b10 (mkChannel 0b11111111 0 0 0) (mkChannel 0b10111111 0 0 0)

        describe "Loops" $ do
            testGet loops (mkChannel 0b00100000 0 0 0) True
            testSet loops False (mkChannel 0b11111111 0 0 0) (mkChannel 0b11011111 0 0 0)

        describe "Volume is constant" $ do
            testGet volumeIsConst (mkChannel 0b00010000 0 0 0) True
            testSet volumeIsConst False (mkChannel 0b11111111 0 0 0) (mkChannel 0b11101111 0 0 0)

        describe "Volume" $ do
            testGet volume (mkChannel 0b00000100 0 0 0) 0b100
            testSet volume 0b100 (mkChannel 0b11111111 0 0 0) (mkChannel 0b11110100 0 0 0)

        describe "Sweep" $ do
            describe "Enabled" $ do
                testGet sweepEnabled (mkChannel 0 0b10000000 0 0) True
                testSet sweepEnabled False (mkChannel 0 0b11111111 0 0) (mkChannel 0 0b01111111 0 0)

            describe "Period" $ do
                testGet sweepPeriod (mkChannel 0 0b01110000 0 0) 0b111
                testSet sweepPeriod 0 (mkChannel 0 0b11111111 0 0) (mkChannel 0 0b10001111 0 0)

            describe "Negate" $ do
                testGet sweepNegate (mkChannel 0 0b00001000 0 0) True
                testSet sweepNegate False (mkChannel 0 0b11111111 0 0) (mkChannel 0 0b11110111 0 0)

            describe "Shift" $ do
                testGet sweepShift (mkChannel 0 0b00000111 0 0) 0b111
                testSet sweepShift 0 (mkChannel 0 0b11111111 0 0) (mkChannel 0 0b11111000 0 0)

        describe "Timer" $ do
            testGet (timer @Pulse) (mkChannel 0 0 0b00001111 0b00000111) 0b11100001111
            testSet (timer @Pulse) 0b101 (mkChannel 0 0 0b11111111 0b11111111) (mkChannel 0 0 0b101 0b11111000)

        describe "Length Counter Load" $ do
            testGet (lengthCounterLoad @Pulse) (mkChannel 0 0 0 0b10001111) 0b10001
            testSet (lengthCounterLoad @Pulse) 0b101 (mkChannel 0 0 0 0b11111111) (mkChannel 0 0 0 0b00101111)

    describe "Triangle" $ do
        describe "Linear Counter Control" $ do
            testGet linearCounterControl (mkChannel 0b10000000 0 0 0) True
            testSet linearCounterControl True (mkChannel 0b00111111 0 0 0) (mkChannel 0b10111111 0 0 0)

        describe "Linear Counter Load" $ do
            testGet linearCounterLoad (mkChannel 0b111111000 0 0 0) 0b1111000
            testSet linearCounterLoad 0b1000001 (mkChannel 0b11111111 0 0 0) (mkChannel 0b11000001 0 0 0)
  where
    testGet getter pulse expected = it "Get" $ get getter pulse `shouldBe` expected
    testSet field newval pulse expected = it "Set" $ set field newval pulse `shouldBe` expected
    mkChannel b0 b1 b2 b3 = fromChannel $ MkChannel b0 b1 b2 b3
