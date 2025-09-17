module CPU.Instructions.INSpec (spec) where

import GHC.Storable
import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = do
    describe "In memory" $ do
        it "Base (Overflow)" $ do
            let setup ptr = writeWord8OffPtr ptr 0x05 0xff
            withStateAndMemorySetup [0xe6, 0x05, 0x00] newCPUState setup $ \cpu ptr -> do
                readWord8OffPtr ptr 0x05 `shouldReturn` 0x00
                getStatusFlagPure Zero cpu `shouldBe` True
    describe "Register X" $ do
        it "Base" $ do
            let st = newCPUState{registerX = 0x10}
            withState [0xe8, 0x00] st $ \cpu -> do
                registerX cpu `shouldBe` 0x11
                getStatusFlagPure Zero cpu `shouldBe` False
                getStatusFlagPure Negative cpu `shouldBe` False

        it "Set Zero (Overflow)" $ do
            let st = newCPUState{registerX = 0xff}
            withState [0xe8, 0x00] st $ \cpu -> do
                registerX cpu `shouldBe` 0x0
                getStatusFlagPure Zero cpu `shouldBe` True
                getStatusFlagPure Negative cpu `shouldBe` False

    describe "Register Y" $ do
        it "Base" $ do
            let st = newCPUState{registerY = 0xfa}
            withState [0xc8, 0x00] st $ \cpu -> do
                registerY cpu `shouldBe` 0xfb
                getStatusFlagPure Zero cpu `shouldBe` False
                getStatusFlagPure Negative cpu `shouldBe` True
