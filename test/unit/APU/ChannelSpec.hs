{-# LANGUAGE TypeApplications #-}

module APU.ChannelSpec (spec) where

import Nes.APU.State.BitField
import Nes.APU.State.Channel
import Nes.APU.State.DMC
import Nes.APU.State.Noise
import Nes.APU.State.Pulse
import Nes.APU.State.Triangle
import Test.Hspec

spec :: Spec
spec = do
    describe "Pulse" $ do
        describe "Duty" $ do
            testGet duty (mkChannel 0b10111111 0 0 0) 0b10
            testSet duty 0b10 (mkChannel 0b11111111 0 0 0) (mkChannel 0b10111111 0 0 0)

        describe "Loops" $ do
            testGet (loops @Pulse) (mkChannel 0b00100000 0 0 0) True
            testSet (loops @Pulse) False (mkChannel 0b11111111 0 0 0) (mkChannel 0b11011111 0 0 0)

        describe "Volume is constant" $ do
            testGet (volumeIsConst @Pulse) (mkChannel 0b00010000 0 0 0) True
            testSet (volumeIsConst @Pulse) False (mkChannel 0b11111111 0 0 0) (mkChannel 0b11101111 0 0 0)

        describe "Volume" $ do
            testGet (volume @Pulse) (mkChannel 0b00000100 0 0 0) 0b100
            testSet (volume @Pulse) 0b100 (mkChannel 0b11111111 0 0 0) (mkChannel 0b11110100 0 0 0)

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

    describe "Noise" $ do
        describe "Noise Mode" $ do
            testGet noiseMode (mkChannel 0 0 0b10000000 0) True
            testSet noiseMode True (mkChannel 0 0 0 0) (mkChannel 0 0 0b10000000 0)

        describe "Noise Period" $ do
            testGet noisePeriod (mkChannel 0 0 0b11110111 0) 0b111
            testSet noisePeriod 0b1010 (mkChannel 0 0 0b11110000 0) (mkChannel 0 0 0b11111010 0)

    describe "DMC" $ do
        describe "IRQ Enable" $ do
            testGet irqEnable (mkChannel 0b10000000 0 0 0) True
            testSet irqEnable True (mkChannel 0 0 0 0) (mkChannel 0b10000000 0 0 0)

        describe "Loop" $ do
            testGet loopsDMC (mkChannel 0b01000000 0 0 0) True
            testSet loopsDMC True (mkChannel 0 0 0 0) (mkChannel 0b01000000 0 0 0)

        describe "Frequency" $ do
            testGet frequency (mkChannel 0b1011 0 0 0) 0b1011
            testSet frequency 0b1100 (mkChannel 0b10000000 0 0 0) (mkChannel 0b10001100 0 0 0)

        describe "Load Counter" $ do
            testGet loadCounter (mkChannel 0 0b01011111 0 0) 0b1011111
            testSet loadCounter 0xff (mkChannel 0 0 0 0) (mkChannel 0 0b01111111 0 0)

        describe "Sample Address" $ do
            testGet sampleAddress (mkChannel 0 0 0b10101010 0) 0b10101010
            testSet sampleAddress 123 (mkChannel 0 0 0 0) (mkChannel 0 0 123 0)

        describe "Sample Length" $ do
            testGet sampleLength (mkChannel 0 0 0 0b10101010) 0b10101010
            testSet sampleLength 0xff (mkChannel 0 0 0 0) (mkChannel 0 0 0 0xff)
  where
    testGet getter pulse expected = it "Get" $ get getter pulse `shouldBe` expected
    testSet field newval pulse expected = it "Set" $ set field newval pulse `shouldBe` expected
    mkChannel b0 b1 b2 b3 = fromChannel $ MkChannel b0 b1 b2 b3
