module CPU.Instructions.LDSpec (spec) where

import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = do
    describe "Register A" $ do
        it "Base" $ do
            let st = newCPUState{registerA = 0x10}
            -- TODO At byte 5, write Y
            cpu <- runAndDump [0x85, 0x05, 0x00]
            -- TODO Check byte Y is 0x10
            registerA cpu `shouldBe` 0x05
