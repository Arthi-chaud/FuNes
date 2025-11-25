module CPU.Instructions.CompareSpec (spec) where

import Internal
import Nes.CPU.State
import Nes.FlagRegister
import Test.Hspec

spec :: Spec
spec = do
    it "Register A (Immediate, Equal)" $ do
        let st = newCPUState{_registerA = 1}
        withState [0xc9, 0x01, 0x00] st $ \st' -> do
            getFlag Carry (_status st') `shouldBe` True
            getFlag Zero (_status st') `shouldBe` True
            getFlag Negative (_status st') `shouldBe` False
    it "Register X (Immediate, Inferior)" $ do
        let st = newCPUState{_registerX = 1}
        withState [0xe0, 0x02, 0x00] st $ \st' -> do
            getFlag Carry (_status st') `shouldBe` False
            getFlag Zero (_status st') `shouldBe` False
            getFlag Negative (_status st') `shouldBe` True
    it "Register Y (Immediate, Greater)" $ do
        let st = newCPUState{_registerY = 3}
        withState [0xc0, 0x02, 0x00] st $ \st' -> do
            getFlag Carry (_status st') `shouldBe` True
            getFlag Zero (_status st') `shouldBe` False
            getFlag Negative (_status st') `shouldBe` False
