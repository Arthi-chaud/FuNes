module Nes.Render (
    render,
) where

import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Foreign
import Nes.Bus
import Nes.Memory
import Nes.Memory.Unsafe ()
import Nes.PPU.Constants
import Nes.PPU.Pointers
import Nes.PPU.State hiding (ScrollRegister (..))
import qualified Nes.PPU.State as Scroll (ScrollRegister (..))
import Nes.Render.Frame
import Nes.Render.Palette
import Nes.Rom

type Palette = (Int, Int, Int, Int)

render :: Frame -> Bus -> IO ()
render frame bus = do
    renderBackground frame bus
    renderSprites frame bus

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

renderBackground :: Frame -> Bus -> IO ()
renderBackground frame bus = do
    let (scrollX, scrollY) =
            let
                scroll = (scrollRegister $ ppuState bus)
             in
                (byteToInt $ Scroll.x scroll, byteToInt $ Scroll.y scroll)
        ppuVram = vram $ ppuPointers bus
    let (nt1, nt2) = case (Nes.PPU.State.mirroring $ ppuState bus, getNametableAddr . controlRegister $ ppuState bus) of
            (Vertical, 0x2000) -> (bsFromSlice ppuVram (0, 0x400), bsFromSlice ppuVram (0x400, 0x800))
            (Vertical, 0x2800) -> (bsFromSlice ppuVram (0, 0x400), bsFromSlice ppuVram (0x400, 0x800))
            (Horizontal, 0x2000) -> (bsFromSlice ppuVram (0, 0x400), bsFromSlice ppuVram (0x400, 0x800))
            (Horizontal, 0x2400) -> (bsFromSlice ppuVram (0, 0x400), bsFromSlice ppuVram (0x400, 0x800))
            (Vertical, 0x2400) -> (bsFromSlice ppuVram (0x400, 0x800), bsFromSlice ppuVram (0, 0x400))
            (Vertical, 0x2C00) -> (bsFromSlice ppuVram (0x400, 0x800), bsFromSlice ppuVram (0, 0x400))
            (Horizontal, 0x2800) -> (bsFromSlice ppuVram (0x400, 0x800), bsFromSlice ppuVram (0, 0x400))
            (Horizontal, 0x2C00) -> (bsFromSlice ppuVram (0x400, 0x800), bsFromSlice ppuVram (0, 0x400))
            _ -> error "Not supported mirror type"
    renderNameTable frame bus nt1 (MkVP{xStart_ = scrollX, yStart_ = scrollY, xEnd_ = 256, yEnd_ = 240}) (-scrollX, -scrollY)
    if scrollX > 0
        then
            renderNameTable frame bus nt2 (MkVP{xStart_ = 0, yStart_ = 0, xEnd_ = scrollX, yEnd_ = 240}) (256 - scrollX, 0)
        else
            when (scrollY > 0) $
                renderNameTable frame bus nt2 (MkVP{xStart_ = 0, yStart_ = 0, xEnd_ = 256, yEnd_ = scrollY}) (0, 240 - scrollY)
  where
    bsFromSlice :: ForeignPtr () -> (Int, Int) -> ByteString
    bsFromSlice vram_ (offset, end) = BS.BS (vram_ `plusForeignPtr` offset) (end - offset + 1)

renderNameTable :: Frame -> Bus -> ByteString -> ViewPort -> (Int, Int) -> IO ()
renderNameTable frame bus nametable vp (shiftX, shiftY) = do
    let chr = chrRom . cartridge $ bus
        bank = addrToInt . getBackgroundPatternAddr . controlRegister . ppuState $ bus
        attrTable = BS.drop 0x3c0 nametable
    forM_ [0 .. 0x3c0] $ \i -> do
        let tileOffset = fromIntegral $ BS.index nametable i
            tileCol = i `mod` 32
            tileRow = i `div` 32
            tile =
                BS.take 16 $
                    BS.drop (bank + tileOffset * 16) chr
        palette <- getBackgroundPalette (ppuPointers bus) attrTable tileCol tileRow
        renderTile palette tile tileCol tileRow
  where
    renderTile (c0, c1, c2, c3) tile tileCol tileRow = forM_ [0 .. 7] $ \y -> do
        let upper = BS.index tile y
            lower = BS.index tile (y + 8)
        forM_ (reverse [0 .. 7]) $ \x -> do
            let value =
                    ((1 .&. (lower `shiftR` (7 - x))) `shiftL` 1)
                        .|. (1 .&. (upper `shiftR` (7 - x)))
            let color = case value of
                    0 -> systemPalette !! c0
                    1 -> systemPalette !! c1
                    2 -> systemPalette !! c2
                    3 -> systemPalette !! c3
                    _ -> error "Bad color index"
                pixelX = tileCol * 8 + x
                pixelY = tileRow * 8 + y
            when
                ( xStart_ vp <= pixelX
                    && pixelX < xEnd_ vp
                    && yStart_ vp <= pixelY
                    && pixelY < yEnd_ vp
                )
                $ frameSetPixel color (pixelX + shiftX, pixelY + shiftY) frame

data ViewPort = MkVP {xEnd_ :: Int, yEnd_ :: Int, xStart_ :: Int, yStart_ :: Int}

getBackgroundPalette :: PPUPointers -> ByteString -> Int -> Int -> IO Palette
getBackgroundPalette ptrs attrTable tileCol tileRow = do
    let attrTableIdx = tileRow `div` 4 * 8 + tileCol `div` 4
        attrByte = fromIntegral $ BS.index attrTable attrTableIdx
    let paletteIdx =
            (.&. 0b11) $ case (tileCol `mod` 4 `div` 2, tileRow `mod` 4 `div` 2) of
                (0, 0) -> attrByte
                (1, 0) -> attrByte `shiftR` 2
                (0, 1) -> attrByte `shiftR` 4
                (1, 1) -> attrByte `shiftR` 6
                _ -> error "Should not happen"
        paletteOffset = Addr $ fromIntegral $ 1 + byteToInt paletteIdx * 4
    c0 <- byteToInt <$> readByte 0 (paletteTable ptrs)
    c1 <- byteToInt <$> readByte paletteOffset (paletteTable ptrs)
    c2 <- byteToInt <$> readByte (paletteOffset + 1) (paletteTable ptrs)
    c3 <- byteToInt <$> readByte (paletteOffset + 2) (paletteTable ptrs)
    return (c0, c1, c2, c3)

getSpritePalette :: PPUPointers -> Int -> IO Palette
getSpritePalette ptrs paletteIdx = do
    let offset = Addr $ fromIntegral $ 0x11 + (paletteIdx * 4)
    c1 <- byteToInt <$> readByte offset (paletteTable ptrs)
    c2 <- byteToInt <$> readByte (offset + 1) (paletteTable ptrs)
    c3 <- byteToInt <$> readByte (offset + 2) (paletteTable ptrs)
    return (0, c1, c2, c3)

-- TODO https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch6.4/src/render/mod.rs#L63
