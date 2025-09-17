module CPU.Instructions.TASpec (spec) where

import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = do
    describe "Register X" $ do
        it "Base" $ do
            let st = newCPUState{registerA = 0x10}
            withState [0xaa, 0x00] st $ \cpu -> do
                registerX cpu `shouldBe` 0x10
                getStatusFlagPure Zero cpu `shouldBe` False
                getStatusFlagPure Negative cpu `shouldBe` False

        it "Set Zero" $ do
            let st = newCPUState{registerA = 0}
            withState [0xaa, 0x00] st $ \cpu -> do
                registerX cpu `shouldBe` 0x0
                getStatusFlagPure Zero cpu `shouldBe` True
                getStatusFlagPure Negative cpu `shouldBe` False
    describe "Register Y" $ do
        it "Base" $ do
            let st = newCPUState{registerA = 0x10}
            withState [0xa8, 0x00] st $ \cpu -> do
                registerY cpu `shouldBe` 0x10
