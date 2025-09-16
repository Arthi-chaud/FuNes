module CPU.Instructions.STSpec (spec) where

import Foreign
import GHC.Storable (readWord8OffPtr, writeWord8OffPtr)
import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = do
    describe "Register A" $ do
        it "Using Zero Page addressing" $ do
            let st = newCPUState{registerA = 0x10}
                setup ptr = return ()
            withStateAndMemorySetup [0x85, 0x05, 0x00] st setup $ \_ ptr -> do
                byte <- readWord8OffPtr ptr 0x05
                byte `shouldBe` 0x10
