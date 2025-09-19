module CPU.Instructions.BranchSpec (spec) where

import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = describe "Branch when" $ do
    describe "Overflow" $ do
        describe "Is Clear" $ do
            testBranch Nothing 0x50
        describe "Is set" $ do
            testBranch (Just Overflow) 0x70
            it "should not branch" $ do
                let program = [0x70, 0x02, 0x00, 0xe8, 0x00]
                withProgram program $ \st' -> do
                    registerX st' `shouldBe` 0
    describe "Zero" $ do
        describe "Is Clear" $ testBranch Nothing 0xd0
        describe "Is Set" $ testBranch (Just Zero) 0xf0
    describe "Negative" $ do
        describe "Is Clear" $ testBranch Nothing 0x10
        describe "Is Set" $ testBranch (Just Negative) 0x30
    describe "Carry" $ do
        describe "Is Clear" $ testBranch Nothing 0x90
        describe "Is Set" $ testBranch (Just Carry) 0xb0
  where
    testBranch Nothing opcode = it "should branch" $ do
        let program = [opcode, 0x02, 0x00, 0xe8, 0x00]
        withState program newCPUState $ \st' -> do
            registerX st' `shouldBe` 1
    testBranch (Just flag) opcode = it "should branch" $ do
        let program = [opcode, 0x02, 0x00, 0xe8, 0x00]
        withState program (setStatusFlagPure flag newCPUState) $ \st' -> do
            registerX st' `shouldBe` 1
