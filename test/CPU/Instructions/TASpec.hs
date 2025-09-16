module CPU.Instructions.TASpec (spec) where

import Nes.CPU.State
import Internal
import Test.Hspec

spec :: Spec
spec = do
    describe "Register X" $ do
        it "Base" $ do
            let st = newCPUState{registerA = 0x10}
            cpu <- runWithStateAndDump st [0xaa, 0x00]
            registerX cpu `shouldBe` 0x10
            getStatusFlagPure Zero cpu `shouldBe` False
            getStatusFlagPure Negative cpu `shouldBe` False

        it "Set Zero" $ do
            let st = newCPUState{registerA = 0}
            cpu <- runWithStateAndDump st [0xaa, 0x00]
            registerX cpu `shouldBe` 0x0
            getStatusFlagPure Zero cpu `shouldBe` True
            getStatusFlagPure Negative cpu `shouldBe` False
