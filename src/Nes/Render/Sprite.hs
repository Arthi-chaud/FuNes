module Nes.Render.Sprite (renderSprites) where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import Nes.Bus
import Nes.Memory
import Nes.PPU.Constants
import Nes.PPU.Pointers
import Nes.PPU.State hiding (ScrollRegister (..))
import Nes.Render.Frame
import Nes.Render.Palette
import Nes.Rom

renderSprites :: Frame -> Bus -> IO ()
renderSprites frame bus = do
    let oam = oamData $ ppuPointers bus
        chr = chrRom . cartridge $ bus
        bank = addrToInt . getSpritePatternAddr . controlRegister . ppuState $ bus
    forM_ (reverse [0, 4 .. oamDataSize - 1]) $ \i -> do
        tileIdx <- byteToInt <$> readByte (fromIntegral i + 1) oam
        tileCol <- byteToInt <$> readByte (fromIntegral i + 3) oam
        tileRow <- byteToInt <$> readByte (fromIntegral i) oam
        attributes <- readByte (fromIntegral i + 2) oam
        let flipVertical = testBit attributes 7
            flipHorizontal = testBit attributes 6
            -- TODO Handle bit 5: priority
            paletteIdx = byteToInt (attributes .&. 0b11)
            tile =
                BS.take 16 $
                    BS.drop (bank + tileIdx * 16) chr
        (_, c1, c2, c3) <- getSpritePalette (ppuPointers bus) paletteIdx
        forM [0 .. 7] $ \y -> do
            let upper = BS.index tile y
                lower = BS.index tile (y + 8)
            forM_ (reverse [0 .. 7]) $ \x -> do
                let value =
                        ((1 .&. (lower `shiftR` (7 - x))) `shiftL` 1)
                            .|. (1 .&. (upper `shiftR` (7 - x)))
                when (value /= 0) $ do
                    let color = case value of
                            1 -> systemPalette !! c1
                            2 -> systemPalette !! c2
                            3 -> systemPalette !! c3
                            _ -> error "Bad color index"
                    let flipCoord base n flip' = if not flip' then base + n else base + 7 - n
                    frameSetPixel
                        color
                        (flipCoord tileCol x flipHorizontal, flipCoord tileRow y flipVertical)
                        frame

getSpritePalette :: PPUPointers -> Int -> IO (Int, Int, Int, Int)
getSpritePalette ptrs paletteIdx = do
    let offset = Addr $ fromIntegral $ 0x11 + (paletteIdx * 4)
    c1 <- byteToInt <$> readByte offset (paletteTable ptrs)
    c2 <- byteToInt <$> readByte (offset + 1) (paletteTable ptrs)
    c3 <- byteToInt <$> readByte (offset + 2) (paletteTable ptrs)
    return (0, c1, c2, c3)

-- TODO https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch6.4/src/render/mod.rs#L63
