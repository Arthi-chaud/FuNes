module CPU.OpenBusSpec (spec) where

import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = do
    -- Inspired https://www.nesdev.org/wiki/Open_bus_behavior#Indirect_indexed_instruction_example
    it "Reading from address causes open bus" $ do
        let program = [0xad, 0x00, 0x73]
            st = newCPUState
        withState program st $ \st' -> do
            registerA st' `shouldBe` 0x73
