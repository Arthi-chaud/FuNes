module CPU.Instructions.ArithSpec (spec) where

import Internal
import Nes.Bus.State
import Nes.CPU.State
import Nes.FlagRegister
import Nes.Memory
import Test.Hspec

spec :: Spec
spec = do
    describe "Subtract with Carry" $ do
        it "Immediate" $ do
            let st = newCPUState{_registerA = 10, _status = setFlag Carry (MkSR 0)}
            withState [0xe9, 0x08, 0x00] st $ \st' -> do
                _registerA st' `shouldBe` 2 -- 10 - 8 - 0
                getFlag Zero (_status st') `shouldBe` False
                getFlag Negative (_status st') `shouldBe` False
                getFlag Carry (_status st') `shouldBe` True
                getFlag Overflow (_status st') `shouldBe` False
        it "Zero Page, set Negative" $ do
            let st = newCPUState{_registerA = 10}
                setup bus = writeByte 11 0x10 (_cpuVram bus)
            withStateAndMemorySetup [0xe5, 0x10, 0x00] st setup $ \st' _ -> do
                _registerA st' `shouldBe` 10 - 11 - 1
                getFlag Negative (_status st') `shouldBe` True
                getFlag Zero (_status st') `shouldBe` False
                getFlag Carry (_status st') `shouldBe` False
                getFlag Overflow (_status st') `shouldBe` False
    describe "Add with Carry" $ do
        it "Immediate, set Overflow" $ do
            let st = newCPUState{_registerA = 127}
            withState [0x69, 0x1, 0x00] st $ \st' -> do
                _registerA st' `shouldBe` 127 + 1
                getFlag Negative (_status st') `shouldBe` True
                getFlag Zero (_status st') `shouldBe` False
                -- that bit should be set only if underflow (?)
                getFlag Carry (_status st') `shouldBe` False
                getFlag Overflow (_status st') `shouldBe` True
    describe "Decrement Register" $ do
        it "In memory (w/ Neg Flag)" $ do
            let setup bus = writeByte 0x00 0x05 (_cpuVram bus)
            withStateAndMemorySetup [0xc6, 0x05, 0x00] newCPUState setup $ \cpu bus -> do
                readByte 0x05 (_cpuVram bus) `shouldReturn` 0xff
                getFlag Negative (_status cpu) `shouldBe` True
        it "Register X (w/ Zero flag)" $ do
            let st = newCPUState{_registerX = 1}
            withState [0xca, 0x00] st $ \cpu -> do
                _registerX cpu `shouldBe` 0
                getFlag Zero (_status cpu) `shouldBe` True
                getFlag Negative (_status cpu) `shouldBe` False

        it "Register Y (w/ Neg flag)" $ do
            let st = newCPUState{_registerY = 0}
            withState [0x88, 0x00] st $ \cpu -> do
                _registerY cpu `shouldBe` (-1)
                getFlag Zero (_status cpu) `shouldBe` False
                getFlag Negative (_status cpu) `shouldBe` True
    describe "Increment Register" $ do
        describe "In memory" $ do
            it "Base (Overflow)" $ do
                let setup bus = writeByte 0xff 0x05 (_cpuVram bus)
                withStateAndMemorySetup [0xe6, 0x05, 0x00] newCPUState setup $ \cpu bus -> do
                    readByte 0x05 (_cpuVram bus) `shouldReturn` 0x00
                    getFlag Zero (_status cpu) `shouldBe` True
        describe "Register X" $ do
            it "Base" $ do
                let st = newCPUState{_registerX = 0x10}
                withState [0xe8, 0x00] st $ \cpu -> do
                    _registerX cpu `shouldBe` 0x11
                    getFlag Zero (_status cpu) `shouldBe` False
                    getFlag Negative (_status cpu) `shouldBe` False

            it "Set Zero (Overflow)" $ do
                let st = newCPUState{_registerX = 0xff}
                withState [0xe8, 0x00] st $ \cpu -> do
                    _registerX cpu `shouldBe` 0x0
                    getFlag Zero (_status cpu) `shouldBe` True
                    getFlag Negative (_status cpu) `shouldBe` False
        it "Register Y" $ do
            let st = newCPUState{_registerY = 0xfa}
            withState [0xc8, 0x00] st $ \cpu -> do
                _registerY cpu `shouldBe` 0xfb
                getFlag Zero (_status cpu) `shouldBe` False
                getFlag Negative (_status cpu) `shouldBe` True
