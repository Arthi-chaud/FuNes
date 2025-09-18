module CPU.Instructions.CMPSpec (spec) where

import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = describe "Compare" $ do
    it "Immediate" $ do
        let st = newCPUState{registerA = 1}
        withState [0xc9, 0x01, 0x00] st $ \st' -> do
            getStatusFlagPure Carry st' `shouldBe` True
            getStatusFlagPure Zero st' `shouldBe` True
            getStatusFlagPure Negative st' `shouldBe` False
