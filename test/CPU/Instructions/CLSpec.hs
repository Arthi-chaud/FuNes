module CPU.Instructions.CLSpec (spec) where

import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = describe "Clear Flag" $ do
    testWithFlag Carry 0x18
    testWithFlag DecimalMode 0xd8
    testWithFlag InteruptDisable 0x58
    testWithFlag Overflow 0xb8
  where
    testWithFlag flag opcode =
        it (show flag) $ do
            let st = setStatusFlagPure flag newCPUState
            withState [opcode, 0x00] st $ \cpu ->
                getStatusFlagPure flag cpu `shouldBe` False
