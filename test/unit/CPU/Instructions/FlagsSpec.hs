module CPU.Instructions.FlagsSpec (spec) where

import Internal
import Nes.CPU.State
import Test.Hspec

spec :: Spec
spec = do
    describe "Clear Flag" $ do
        testClearFlag Carry 0x18
        testClearFlag DecimalMode 0xd8
        testClearFlag InteruptDisable 0x58
        testClearFlag Overflow 0xb8
    describe "Set Flag" $ do
        testSetFlag Carry 0x38
        testSetFlag DecimalMode 0xf8
        testSetFlag InteruptDisable 0x78
  where
    testSetFlag flag opcode =
        it (show flag) $ do
            withProgram [opcode, 0x00] $ \cpu ->
                getStatusFlag flag cpu `shouldBe` True
    testClearFlag flag opcode =
        it (show flag) $ do
            let st = setStatusFlag flag newCPUState
            withState [opcode, 0x00] st $ \cpu ->
                getStatusFlag flag cpu `shouldBe` False
