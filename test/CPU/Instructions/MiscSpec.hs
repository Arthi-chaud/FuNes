module CPU.Instructions.MiscSpec (spec) where

import HNes.CPU.State
import Internal
import Test.Hspec

spec :: Spec
spec = do
    it "5 Ops" $ do
        cpu <- runAndDump [0xa9, 0xc0, 0xaa, 0xe8, 0x00]
        registerX cpu `shouldBe` 0xc1
