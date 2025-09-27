module CPU.Instructions.MiscSpec (spec) where

import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = do
    it "5 Ops" $ withProgram [0xa9, 0xc0, 0xaa, 0xe8, 0x00] $ \cpu ->
        registerX cpu `shouldBe` 0xc1
