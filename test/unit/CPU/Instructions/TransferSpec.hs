module CPU.Instructions.TransferSpec (spec) where

import Internal
import Nes.CPU.State
import Nes.FlagRegister
import Test.Hspec

spec :: Spec
spec = do
    describe "From Register A" $ do
        describe "Register X" $ do
            it "Base" $ do
                let st = newCPUState{registerA = 0x10}
                withState [0xaa, 0x00] st $ \cpu -> do
                    registerX cpu `shouldBe` 0x10
                    getFlag Zero (status cpu) `shouldBe` False
                    getFlag Negative (status cpu) `shouldBe` False
            it "Set Zero" $ do
                let st = newCPUState{registerA = 0}
                withState [0xaa, 0x00] st $ \cpu -> do
                    registerX cpu `shouldBe` 0x0
                    getFlag Zero (status cpu) `shouldBe` True
                    getFlag Negative (status cpu) `shouldBe` False
        it "Register Y" $ do
            let st = newCPUState{registerA = 0x10}
            withState [0xa8, 0x00] st $ \cpu -> do
                registerY cpu `shouldBe` 0x10
    describe "From Register X" $ do
        it "Register A" $ do
            let st = newCPUState{registerX = 10}
            withState [0x8a, 0x00] st $ \cpu -> do
                registerA cpu `shouldBe` 10
        it "Register S" $ do
            let st = newCPUState{registerX = 10}
            withState [0x9a, 0x00] st $ \cpu -> do
                registerS cpu `shouldBe` 10
    describe "From Register Y" $ do
        it "Register A" $ do
            let st = newCPUState{registerY = 10}
            withState [0x98, 0x00] st $ \cpu -> do
                registerA cpu `shouldBe` 10
    describe "From Register S" $ do
        it "Register X" $ do
            let st = newCPUState{registerS = 10}
            withState [0xba, 0x00] st $ \cpu -> do
                registerX cpu `shouldBe` 10
