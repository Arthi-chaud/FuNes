module RomSpec (spec) where

import qualified Data.ByteString as BS
import Nes.Memory
import Nes.Rom
import Test.Hspec

spec :: Spec
spec = it "Parse iNES file" $ do
    rom <- do
        res <- fromFile "test/assets/rom.nes"
        either fail return res

    mapper rom `shouldBe` Byte 0
    mirroring rom `shouldBe` Horizontal
    BS.length (prgRom rom) `shouldBe` 16384
    BS.index (prgRom rom) 0 `shouldBe` 76
    BS.length (chrRom rom) `shouldBe` 8192
    BS.index (chrRom rom) 0 `shouldBe` 0
