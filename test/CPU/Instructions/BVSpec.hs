module CPU.Instructions.BVSpec (spec) where

import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = describe "Branch when" $ do
    describe "Overflow is set" $ do
        it "should branch" $ do
            let st = setStatusFlagPure Overflow newCPUState
                program = [0x70, 0x02, 0x00, 0xe8, 0x00]
            withState program st $ \st' -> do
                registerX st' `shouldBe` 1
        it "should not branch" $ do
            let program = [0x70, 0x02, 0x00, 0xe8, 0x00]
            withProgram program $ \st' -> do
                registerX st' `shouldBe` 0
    describe "Overflow is clear" $ do
        it "should branch" $ do
            let program = [0x50, 0x02, 0x00, 0xe8, 0x00]
            withProgram program $ \st' -> do
                registerX st' `shouldBe` 1
