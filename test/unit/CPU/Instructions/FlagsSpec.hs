module CPU.Instructions.FlagsSpec (spec) where

import Internal
import Nes.CPU.State
import Nes.FlagRegister
import Test.Hspec

spec :: Spec
spec = do
    describe "Clear Flag" $ do
        testClearFlag Carry 0x18
        testClearFlag DecimalMode 0xd8
        testClearFlag InterruptDisable 0x58
        testClearFlag Overflow 0xb8
    describe "Set Flag" $ do
        testSetFlag Carry 0x38
        testSetFlag DecimalMode 0xf8
        testSetFlag InterruptDisable 0x78
  where
    testSetFlag flag opcode =
        it (show flag) $ do
            withProgram [opcode, 0x00] $ \cpu ->
                getFlag flag (status cpu) `shouldBe` True
    testClearFlag flag opcode =
        it (show flag) $ do
            let st = modifyStatusRegister (setFlag flag) newCPUState
            withState [opcode, 0x00] st $ \cpu ->
                getFlag flag (status cpu) `shouldBe` False
