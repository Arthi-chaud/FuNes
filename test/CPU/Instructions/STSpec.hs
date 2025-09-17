module CPU.Instructions.STSpec (spec) where

import GHC.Storable (readWord8OffPtr)
import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = do
    it "Register A" $ do
        let st = newCPUState{registerA = 0x10}
        testWithState st 0x85
    it "Register X" $ do
        let st = newCPUState{registerX = 0x10}
        testWithState st 0x86
    it "Register Y" $ do
        let st = newCPUState{registerY = 0x10}
        testWithState st 0x84
  where
    testWithState st opcode = do
        let setup _ = return ()
        withStateAndMemorySetup [opcode, 0x05, 0x00] st setup $ \_ ptr -> do
            byte <- readWord8OffPtr ptr 0x05
            byte `shouldBe` 0x10
