module Render (
    frameWidth,
    frameHeight,
    frameLength,
    newFrame,
    Frame (..),
    render,
) where

import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Ix (Ix (inRange))
import Debug.Trace (trace, traceShow)
import Foreign
import GHC.Exts (Ptr (Ptr))
import GHC.ForeignPtr
import Nes.Bus
import Nes.Internal
import Nes.Memory
import Nes.Memory.Unsafe ()
import Nes.PPU.Constants
import Nes.PPU.Pointers hiding (chrRom)
import Nes.PPU.State
import Nes.Rom

newtype Frame = MkF {unF :: MemoryPointer}

frameWidth :: Int
frameWidth = 256

frameHeight :: Int
frameHeight = 240

frameLength :: Int
frameLength = frameWidth * frameHeight * 3

newFrame :: IO Frame
newFrame = MkF <$> callocForeignPtr frameLength

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
                (byteToInt $ x scroll, byteToInt $ y scroll)
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

frameSetPixel :: (Word8, Word8, Word8) -> (Int, Int) -> Frame -> IO ()
frameSetPixel (colorR, colorG, colorB) (x, y) (MkF fptr) = do
    let base = y * 3 * frameWidth + x * 3
    when (inRange (0, frameLength) (base + 2)) $ do
        -- Note: we don't use 'writeByte' because base + 2 might overflow
        unsafeWithForeignPtr (castForeignPtr fptr) $ \ptr -> do
            pokeByteOff ptr base colorR
            pokeByteOff ptr (base + 1) colorG
            pokeByteOff ptr (base + 2) colorB

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

systemPalette :: [(Word8, Word8, Word8)]
systemPalette =
    [ (0x80, 0x80, 0x80)
    , (0x00, 0x3D, 0xA6)
    , (0x00, 0x12, 0xB0)
    , (0x44, 0x00, 0x96)
    , (0xA1, 0x00, 0x5E)
    , (0xC7, 0x00, 0x28)
    , (0xBA, 0x06, 0x00)
    , (0x8C, 0x17, 0x00)
    , (0x5C, 0x2F, 0x00)
    , (0x10, 0x45, 0x00)
    , (0x05, 0x4A, 0x00)
    , (0x00, 0x47, 0x2E)
    , (0x00, 0x41, 0x66)
    , (0x00, 0x00, 0x00)
    , (0x05, 0x05, 0x05)
    , (0x05, 0x05, 0x05)
    , (0xC7, 0xC7, 0xC7)
    , (0x00, 0x77, 0xFF)
    , (0x21, 0x55, 0xFF)
    , (0x82, 0x37, 0xFA)
    , (0xEB, 0x2F, 0xB5)
    , (0xFF, 0x29, 0x50)
    , (0xFF, 0x22, 0x00)
    , (0xD6, 0x32, 0x00)
    , (0xC4, 0x62, 0x00)
    , (0x35, 0x80, 0x00)
    , (0x05, 0x8F, 0x00)
    , (0x00, 0x8A, 0x55)
    , (0x00, 0x99, 0xCC)
    , (0x21, 0x21, 0x21)
    , (0x09, 0x09, 0x09)
    , (0x09, 0x09, 0x09)
    , (0xFF, 0xFF, 0xFF)
    , (0x0F, 0xD7, 0xFF)
    , (0x69, 0xA2, 0xFF)
    , (0xD4, 0x80, 0xFF)
    , (0xFF, 0x45, 0xF3)
    , (0xFF, 0x61, 0x8B)
    , (0xFF, 0x88, 0x33)
    , (0xFF, 0x9C, 0x12)
    , (0xFA, 0xBC, 0x20)
    , (0x9F, 0xE3, 0x0E)
    , (0x2B, 0xF0, 0x35)
    , (0x0C, 0xF0, 0xA4)
    , (0x05, 0xFB, 0xFF)
    , (0x5E, 0x5E, 0x5E)
    , (0x0D, 0x0D, 0x0D)
    , (0x0D, 0x0D, 0x0D)
    , (0xFF, 0xFF, 0xFF)
    , (0xA6, 0xFC, 0xFF)
    , (0xB3, 0xEC, 0xFF)
    , (0xDA, 0xAB, 0xEB)
    , (0xFF, 0xA8, 0xF9)
    , (0xFF, 0xAB, 0xB3)
    , (0xFF, 0xD2, 0xB0)
    , (0xFF, 0xEF, 0xA6)
    , (0xFF, 0xF7, 0x9C)
    , (0xD7, 0xE8, 0x95)
    , (0xA6, 0xED, 0xAF)
    , (0xA2, 0xF2, 0xDA)
    , (0x99, 0xFF, 0xFC)
    , (0xDD, 0xDD, 0xDD)
    , (0x11, 0x11, 0x11)
    , (0x11, 0x11, 0x11)
    ]
