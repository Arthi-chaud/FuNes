module CPU.Instructions.ArithSpec (spec) where

import Internal
import Nes.CPU.State
import Nes.Memory
import Test.Hspec

spec :: Spec
spec = do
    describe "Subtract with Carry" $ do
        it "Immediate" $ do
            let st = setStatusFlagPure Carry newCPUState{registerA = 10}
            withState [0xe9, 0x08, 0x00] st $ \st' -> do
                registerA st' `shouldBe` 2 -- 10 - 8 - 0
                getStatusFlagPure Zero st' `shouldBe` False
                getStatusFlagPure Negative st' `shouldBe` False
                getStatusFlagPure Carry st' `shouldBe` True
                getStatusFlagPure Overflow st' `shouldBe` False
        it "Zero Page, set Negative" $ do
            let st = newCPUState{registerA = 10}
                setup = writeByte 11 0x10
            withStateAndMemorySetup [0xe5, 0x10, 0x00] st setup $ \st' _ -> do
                registerA st' `shouldBe` 10 - 11 - 1
                getStatusFlagPure Negative st' `shouldBe` True
                getStatusFlagPure Zero st' `shouldBe` False
                getStatusFlagPure Carry st' `shouldBe` True
                getStatusFlagPure Overflow st' `shouldBe` False
    describe "Add with Carry" $ do
        it "Immediate, set Overflow" $ do
            let st = newCPUState{registerA = 127}
            withState [0x69, 0x1, 0x00] st $ \st' -> do
                registerA st' `shouldBe` 127 + 1
                getStatusFlagPure Negative st' `shouldBe` True
                getStatusFlagPure Zero st' `shouldBe` False
                -- that bit should be set only if underflow (?)
                getStatusFlagPure Carry st' `shouldBe` False
                getStatusFlagPure Overflow st' `shouldBe` True
    describe "Decrement Register" $ do
        it "In memory (w/ Neg Flag)" $ do
            let setup = writeByte 0x00 0x05
            withStateAndMemorySetup [0xc6, 0x05, 0x00] newCPUState setup $ \cpu bus -> do
                readByte 0x05 bus `shouldReturn` 0xff
                getStatusFlagPure Negative cpu `shouldBe` True
        it "Register X (w/ Zero flag)" $ do
            let st = newCPUState{registerX = 1}
            withState [0xca, 0x00] st $ \cpu -> do
                registerX cpu `shouldBe` 0
                getStatusFlagPure Zero cpu `shouldBe` True
                getStatusFlagPure Negative cpu `shouldBe` False

        it "Register Y (w/ Neg flag)" $ do
            let st = newCPUState{registerY = 0}
            withState [0x88, 0x00] st $ \cpu -> do
                registerY cpu `shouldBe` (-1)
                getStatusFlagPure Zero cpu `shouldBe` False
                getStatusFlagPure Negative cpu `shouldBe` True
    describe "Increment Register" $ do
        describe "In memory" $ do
            it "Base (Overflow)" $ do
                let setup = writeByte 0xff 0x05
                withStateAndMemorySetup [0xe6, 0x05, 0x00] newCPUState setup $ \cpu bus -> do
                    readByte 0x05 bus `shouldReturn` 0x00
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
        it "Register Y" $ do
            let st = newCPUState{registerY = 0xfa}
            withState [0xc8, 0x00] st $ \cpu -> do
                registerY cpu `shouldBe` 0xfb
                getStatusFlagPure Zero cpu `shouldBe` False
                getStatusFlagPure Negative cpu `shouldBe` True
