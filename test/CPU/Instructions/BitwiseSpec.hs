module CPU.Instructions.BitwiseSpec (spec) where

import GHC.Storable (writeWord8OffPtr)
import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = do
    describe "Bit Test" $ do
        it "sets carry" $ do
            let program = [0x24, 0x06, 0x00]
                setup ptr = writeWord8OffPtr ptr 0x06 0b0001000
                st = newCPUState{registerA = 0b0010000}
            withStateAndMemorySetup program st setup $ \st' _ -> do
                getStatusFlagPure Zero st' `shouldBe` True
                getStatusFlagPure Overflow st' `shouldBe` False
                getStatusFlagPure Negative st' `shouldBe` False
        it "sets overflow" $ do
            let program = [0x24, 0x06, 0x00]
                setup ptr = writeWord8OffPtr ptr 0x06 0b01100000
                st = newCPUState{registerA = 0b00100000}
            withStateAndMemorySetup program st setup $ \st' _ -> do
                getStatusFlagPure Zero st' `shouldBe` False
                getStatusFlagPure Overflow st' `shouldBe` True
                getStatusFlagPure Negative st' `shouldBe` False
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
            getStatusFlagPure Carry st' `shouldBe` False
    it "Rotate Right" $ do
        let program = [0x6a, 0x00]
            st = setStatusFlagPure Carry (newCPUState{registerA = 0b00010000})
        withState program st $ \st' -> do
            registerA st' `shouldBe` 0b10001000
            getStatusFlagPure Carry st' `shouldBe` False
