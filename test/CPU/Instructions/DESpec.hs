module CPU.Instructions.DESpec (spec) where

import GHC.Storable
import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = describe "Decrement Register" $ do
    it "In memory (w/ Neg Flag)" $ do
        let setup ptr = writeWord8OffPtr ptr 0x05 0x00
        withStateAndMemorySetup [0xc6, 0x05, 0x00] newCPUState setup $ \cpu ptr -> do
            readWord8OffPtr ptr 0x05 `shouldReturn` 0xff
            getStatusFlagPure Negative cpu `shouldBe` True
    it "Register X (w/ Zero flag)" $ do
        let st = newCPUState{registerX = 1}
        withState [0xca, 0x00] st $ \cpu -> do
            registerX cpu `shouldBe` 0
            getStatusFlagPure Zero cpu `shouldBe` True
            getStatusFlagPure Negative cpu `shouldBe` False

    it "Register Y (w/ Neg flag)" $ do
        let st = newCPUState{registerY = 0}
        withState [0x88, 0x00] st $ \cpu -> do
            registerY cpu `shouldBe` (-1)
            getStatusFlagPure Zero cpu `shouldBe` False
            getStatusFlagPure Negative cpu `shouldBe` True
