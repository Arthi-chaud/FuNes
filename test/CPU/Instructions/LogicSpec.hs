module CPU.Instructions.LogicSpec (spec) where

import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = describe "Logic" $ do
    it "And" $ do
        let program = [0x29, 0x1, 0x00]
            st = newCPUState{registerA = 0x2}
        withState program st $ \st' -> do
            registerA st' `shouldBe` 0
            getStatusFlagPure Zero st' `shouldBe` True
    it "Or" $ do
        let program = [0x09, 0x2, 0x00]
            st = newCPUState{registerA = 0x1}
        withState program st $ \st' -> do
            registerA st' `shouldBe` 3
            getStatusFlagPure Zero st' `shouldBe` False
    it "Xor" $ do
        let program = [0x49, 0x2, 0x00]
            st = newCPUState{registerA = 0x2}
        withState program st $ \st' -> do
            registerA st' `shouldBe` 0
            getStatusFlagPure Zero st' `shouldBe` True
    it "Rotate Left" $ do
        let program = [0x2a, 0x00]
            st = setStatusFlagPure Carry (newCPUState{registerA = 0b00010000})
        withState program st $ \st' -> do
            registerA st' `shouldBe` 0b00100001

    it "Rotate Right" $ do
        let program = [0x6a, 0x00]
            st = setStatusFlagPure Carry (newCPUState{registerA = 0b00010000})
        withState program st $ \st' -> do
            registerA st' `shouldBe` 0b10001000
