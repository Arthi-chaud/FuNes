module CPU.Instructions.AccessSpec (spec) where

import Internal
import Nes.CPU.State
import Nes.Memory
import Test.Hspec

spec :: Spec
spec = do
    describe "Load Value to Register" $ do
        describe "Register A" $ do
            it "Base" $
                withProgram [0xa9, 0x05, 0x00] $ \cpu -> do
                    registerA cpu `shouldBe` 0x05
                    getStatusFlagPure Zero cpu `shouldBe` False
                    getStatusFlagPure Negative cpu `shouldBe` False

            it "Set Zero flag" $
                withProgram [0xa9, 0x00, 0x00] $ \cpu -> do
                    registerA cpu `shouldBe` 0x00
                    getStatusFlagPure Zero cpu `shouldBe` True
                    getStatusFlagPure Negative cpu `shouldBe` False

            it "Load from memory (Zero Page)" $ do
                let setup = writeByte 0x55 0x10
                withMemorySetup [0xa5, 0x10, 0x00] setup $ \cpu _ -> do
                    registerA cpu `shouldBe` 0x55
        describe "Register X" $ do
            it "Immediate" $
                withProgram [0xa2, 0xff, 0x00] $ \cpu -> do
                    registerX cpu `shouldBe` 0xff
                    getStatusFlagPure Negative cpu `shouldBe` True
        describe "Register Y" $ do
            it "Immediate" $
                withProgram [0xa0, 0x05, 0x00] $ \cpu -> do
                    registerY cpu `shouldBe` 0x05
                    getStatusFlagPure Zero cpu `shouldBe` False
                    getStatusFlagPure Negative cpu `shouldBe` False
    describe "Store value in memory" $ do
        it "Register A" $ do
            let st = newCPUState{registerA = 0x10}
            testStore st 0x85
        it "Register X" $ do
            let st = newCPUState{registerX = 0x10}
            testStore st 0x86
        it "Register Y" $ do
            let st = newCPUState{registerY = 0x10}
            testStore st 0x84
  where
    testStore st opcode = do
        withStateAndMemorySetup [opcode, 0x05, 0x00] st (const $ pure ()) $ \_ bus -> do
            byte <- readByte 0x05 bus
            byte `shouldBe` 0x10
