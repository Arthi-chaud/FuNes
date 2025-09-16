module CPU.Instructions.INSpec (spec) where

import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = do
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
