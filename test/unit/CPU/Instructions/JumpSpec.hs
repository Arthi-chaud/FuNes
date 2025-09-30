module CPU.Instructions.JumpSpec (spec) where

import Internal
import Nes.Bus
import Nes.Bus.Monad
import Nes.CPU.State
import Nes.Memory
import Test.Hspec

spec :: Spec
spec = describe "Jump" $ do
    it "Absolute" $ do
        let setup bus = writeAddr 0x00e8 0x05 (cpuVram bus)
        withStateAndMemorySetup [0x4c, 0x05, 0x00, 0x00] newCPUState setup $
            \cpu _ -> getRegisterPure X cpu `shouldBe` 1

    it "Indirect" $ do
        let setup bus = runBusM bus $ do
                -- The pointer to the destination
                writeAddr 0x08 0x05 ()
                -- The jump destination
                writeAddr 0x00c8 0x08 ()
        withStateAndMemorySetup [0x6c, 0x05, 0x00] newCPUState setup $
            \cpu _ -> getRegisterPure Y cpu `shouldBe` 1
