module CPU.Instructions.LDASpec (spec) where

import HNes.CPU.State
import Internal
import Test.Hspec

spec :: Spec
spec = do
    it "Base" $ do
        cpu <- runAndDump [0xa9, 0x05, 0x00]
        registerA cpu `shouldBe` 0x05
        getStatusFlagPure Zero cpu `shouldBe` False
        getStatusFlagPure Negative cpu `shouldBe` False

    it "Set Zero flag" $ do
        cpu <- runAndDump [0xa9, 0x00, 0x00]
        registerA cpu `shouldBe` 0x00
        getStatusFlagPure Zero cpu `shouldBe` True
        getStatusFlagPure Negative cpu `shouldBe` False
