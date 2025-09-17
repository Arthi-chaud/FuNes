module CPU.Instructions.SESpec (spec) where

import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = describe "Set Flag" $ do
    testWithFlag Carry 0x38
    testWithFlag DecimalMode 0xf8
    testWithFlag InteruptDisable 0x78
  where
    testWithFlag flag opcode =
        it (show flag) $ do
            withProgram [opcode, 0x00] $ \cpu ->
                getStatusFlagPure flag cpu `shouldBe` True
