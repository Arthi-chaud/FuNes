module CPU.Instructions.JMPSpec (spec) where

import GHC.Storable
import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = describe "Jump" $ do
    it "Absolute" $ do
        let setup ptr = do
                writeWord8OffPtr ptr 0x05 0xe8
                writeWord8OffPtr ptr 0x06 0x00
        withStateAndMemorySetup [0x4c, 0x05, 0x00, 0x00] newCPUState setup $
            \cpu _ -> getRegisterPure X cpu `shouldBe` 1

    it "Indirect" $ do
        let setup ptr = do
                -- The pointer to the destination
                writeWord8OffPtr ptr 0x05 0x08
                writeWord8OffPtr ptr 0x06 0x00
                -- The jump destination
                writeWord8OffPtr ptr 0x08 0xc8
                writeWord8OffPtr ptr 0x09 0x00
        withStateAndMemorySetup [0x6c, 0x05, 0x00] newCPUState setup $
            \cpu _ -> getRegisterPure Y cpu `shouldBe` 1
