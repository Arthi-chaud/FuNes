module CPU.Instructions.CompareSpec (spec) where

import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = do
    it "Register A (Immediate, Equal)" $ do
        let st = newCPUState{registerA = 1}
        withState [0xc9, 0x01, 0x00] st $ \st' -> do
            getStatusFlagPure Carry st' `shouldBe` True
            getStatusFlagPure Zero st' `shouldBe` True
            getStatusFlagPure Negative st' `shouldBe` False
    it "Register X (Immediate, Inferior)" $ do
        let st = newCPUState{registerX = 1}
        withState [0xe0, 0x02, 0x00] st $ \st' -> do
            getStatusFlagPure Carry st' `shouldBe` False
            getStatusFlagPure Zero st' `shouldBe` False
            getStatusFlagPure Negative st' `shouldBe` True
    it "Register Y (Immediate, Greater)" $ do
        let st = newCPUState{registerY = 3}
        withState [0xc0, 0x02, 0x00] st $ \st' -> do
            getStatusFlagPure Carry st' `shouldBe` True
            getStatusFlagPure Zero st' `shouldBe` False
            getStatusFlagPure Negative st' `shouldBe` False
