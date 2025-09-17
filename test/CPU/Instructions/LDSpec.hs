module CPU.Instructions.LDSpec (spec) where

import Foreign
import GHC.Storable
import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = do
    describe "Register A" $ do
        it "Base" $
            withProgram [0xa9, 0x05, 0x00] $ \cpu -> do
                registerA cpu `shouldBe` 0x05
                getStatusFlagPure Zero cpu `shouldBe` False
                getStatusFlagPure Negative cpu `shouldBe` False

        it "Set Zero flag" $
            withProgram [0xa9, 0x00, 0x00] $ \cpu -> do
                registerA cpu `shouldBe` 0x00
                getStatusFlagPure Zero cpu `shouldBe` True
                getStatusFlagPure Negative cpu `shouldBe` False

        it "Load from memory (Zero Page)" $ do
            let setup ptr = writeWord8OffPtr (castPtr ptr) 0x10 0x55
            withMemorySetup [0xa5, 0x10, 0x00] setup $ \cpu _ -> do
                registerA cpu `shouldBe` 0x55
    describe "Register X" $ do
        it "Immediate" $
            withProgram [0xa2, 0xff, 0x00] $ \cpu -> do
                registerX cpu `shouldBe` 0xff
                getStatusFlagPure Negative cpu `shouldBe` True
    describe "Register Y" $ do
        it "Immediate" $
            withProgram [0xa0, 0x05, 0x00] $ \cpu -> do
                registerY cpu `shouldBe` 0x05
                getStatusFlagPure Zero cpu `shouldBe` False
                getStatusFlagPure Negative cpu `shouldBe` False
