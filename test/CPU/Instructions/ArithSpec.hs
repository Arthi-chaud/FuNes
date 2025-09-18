module CPU.Instructions.ArithSpec (spec) where

import GHC.Storable (writeWord8OffPtr)
import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = do
    describe "Subtract with Carry" $ do
        it "Immediate" $ do
            let st = setStatusFlagPure Carry newCPUState{registerA = 10}
            withState [0xe9, 0x08, 0x00] st $ \st' -> do
                registerA st' `shouldBe` 2 -- 10 - 8 - 0
                getStatusFlagPure Zero st' `shouldBe` False
                getStatusFlagPure Negative st' `shouldBe` False
                getStatusFlagPure Carry st' `shouldBe` True
                getStatusFlagPure Overflow st' `shouldBe` False
        it "Zero Page, set Negative" $ do
            let st = newCPUState{registerA = 10}
                setup ptr = writeWord8OffPtr ptr 0x10 11
            withStateAndMemorySetup [0xe5, 0x10, 0x00] st setup $ \st' _ -> do
                registerA st' `shouldBe` 10 - 11 - 1
                getStatusFlagPure Negative st' `shouldBe` True
                getStatusFlagPure Zero st' `shouldBe` False
                getStatusFlagPure Carry st' `shouldBe` True
                getStatusFlagPure Overflow st' `shouldBe` False
    describe "Subtract with Carry" $ do
        it "Immediate, set Overflow" $ do
            let st = newCPUState{registerA = 127}
            withState [0x69, 0x1, 0x00] st $ \st' -> do
                registerA st' `shouldBe` 127 + 1
                getStatusFlagPure Negative st' `shouldBe` True
                getStatusFlagPure Zero st' `shouldBe` False
                -- that bit should be set only if underflow (?)
                getStatusFlagPure Carry st' `shouldBe` False
                getStatusFlagPure Overflow st' `shouldBe` True
