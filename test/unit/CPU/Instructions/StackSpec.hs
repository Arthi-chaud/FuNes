module CPU.Instructions.StackSpec (spec) where

import Internal
import Nes.Bus.Constants
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = do
    it "Push and Pull Register A" $ do
        -- Push Reg A, set it to 0x10, and restore it from stack
        let program = [0x48, 0xa9, 0x10, 0x68, 0x00]
            st = newCPUState{registerA = 1}
        withState program st $ \st' -> do
            registerA st' `shouldBe` 1
            registerS st' `shouldBe` stackReset

    it "Push and Pull Status Flag" $ do
        -- Push Status, clear carry bit and restore
        let program = [0x08, 0x18, 0x28, 0x00]
            st = setStatusFlagPure Carry newCPUState
        withState program st $ \st' -> do
            getStatusFlagPure Carry st' `shouldBe` True
            registerS st' `shouldBe` stackReset
