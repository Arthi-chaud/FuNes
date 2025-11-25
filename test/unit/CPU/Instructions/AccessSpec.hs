module CPU.Instructions.AccessSpec (spec) where

import Internal
import Nes.Bus.State (BusState (_cpuVram, _cycles))
import Nes.CPU.State
import Nes.FlagRegister
import Nes.Memory
import Test.Hspec

spec :: Spec
spec = do
    describe "Load Value to Register" $ do
        describe "Register A" $ do
            it "Base" $ do
                let setup _ = pure ()
                withStateAndMemorySetup [0xa9, 0x05, 0x00] newCPUState setup $ \cpu bus -> do
                    _cycles bus `shouldBe` 2
                    _registerA cpu `shouldBe` 0x05

                    getFlag Zero (_status cpu) `shouldBe` False
                    getFlag Negative (_status cpu) `shouldBe` False

            it "Set Zero flag" $
                withProgram [0xa9, 0x00, 0x00] $ \cpu -> do
                    _registerA cpu `shouldBe` 0x00
                    getFlag Zero (_status cpu) `shouldBe` True
                    getFlag Negative (_status cpu) `shouldBe` False

            it "Load from memory (Zero Page)" $ do
                let setup bus = writeByte 0x55 0x10 (_cpuVram bus)
                withMemorySetup [0xa5, 0x10, 0x00] setup $ \cpu _ -> do
                    _registerA cpu `shouldBe` 0x55
        describe "Register X" $ do
            it "Immediate" $
                withProgram [0xa2, 0xff, 0x00] $ \cpu -> do
                    _registerX cpu `shouldBe` 0xff
                    getFlag Negative (_status cpu) `shouldBe` True
        describe "Register Y" $ do
            it "Immediate" $
                withProgram [0xa0, 0x05, 0x00] $ \cpu -> do
                    _registerY cpu `shouldBe` 0x05
                    getFlag Zero (_status cpu) `shouldBe` False
                    getFlag Negative (_status cpu) `shouldBe` False
    describe "Store value in memory" $ do
        it "Register A" $ do
            let st = newCPUState{_registerA = 0x10}
            testStore st 0x85
        it "Register X" $ do
            let st = newCPUState{_registerX = 0x10}
            testStore st 0x86
        it "Register Y" $ do
            let st = newCPUState{_registerY = 0x10}
            testStore st 0x84
  where
    testStore st opcode = do
        withStateAndMemorySetup [opcode, 0x05, 0x00] st (const $ pure ()) $ \_ bus -> do
            byte <- readByte 0x05 (_cpuVram bus)
            byte `shouldBe` 0x10
