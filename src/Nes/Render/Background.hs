module Nes.Render.Background (renderBackground) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Foreign
import Nes.Bus
import Nes.Memory
import Nes.Memory.Unsafe ()
import Nes.PPU.Pointers
import Nes.PPU.State hiding (ScrollRegister (..))
import qualified Nes.PPU.State as Scroll (ScrollRegister (..))
import Nes.Render.Frame2
import Nes.Render.Monad
import qualified Nes.Render.Monad as Render
import Nes.Render.Palette
import Nes.Rom

renderBackground :: Bus -> Render DirtyFrame BGDrawn r ()
renderBackground bus = Render.do
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
    renderNameTable bus nt1 (MkVP{xStart_ = scrollX, yStart_ = scrollY, xEnd_ = 256, yEnd_ = 240}) (-scrollX, -scrollY)
    if scrollX > 0
        then
            renderNameTable bus nt2 (MkVP{xStart_ = 0, yStart_ = 0, xEnd_ = scrollX, yEnd_ = 240}) (256 - scrollX, 0)
        else
            if scrollY > 0
                then
                    renderNameTable bus nt2 (MkVP{xStart_ = 0, yStart_ = 0, xEnd_ = 256, yEnd_ = scrollY}) (0, 240 - scrollY)
                else
                    unsafeStep
  where
    bsFromSlice :: ForeignPtr () -> (Int, Int) -> ByteString
    bsFromSlice vram_ (offset, end) = BS.BS (vram_ `plusForeignPtr` offset) (end - offset + 1)

renderNameTable :: Bus -> ByteString -> ViewPort -> (Int, Int) -> Render a b r ()
renderNameTable bus nametable vp (shiftX, shiftY) = Render.do
    let chr = chrRom . cartridge $ bus
        bank = addrToInt . getBackgroundPatternAddr . controlRegister . ppuState $ bus
        attrTable = BS.drop 0x3c0 nametable
    forR [0 .. 0x3c0] $ \i -> Render.do
        let tileOffset = fromIntegral $ BS.index nametable i
            tileCol = i `mod` 32
            tileRow = i `div` 32
            tile =
                BS.take 16 $
                    BS.drop (bank + tileOffset * 16) chr
        palette <- liftIO $ getBackgroundPalette (ppuPointers bus) attrTable tileCol tileRow
        renderTile palette tile tileCol tileRow
    unsafeStep
  where
    renderTile (c0, c1, c2, c3) tile tileCol tileRow = forR [0 .. 7] $ \y -> do
        let upper = BS.index tile y
            lower = BS.index tile (y + 8)
        forR (reverse [0 .. 7]) $ \x -> do
            let value =
                    ((1 .&. (lower `shiftR` (7 - x))) `shiftL` 1)
                        .|. (1 .&. (upper `shiftR` (7 - x)))
            let color = case value of
                    0 -> systemPalette !! c0
                    1 -> systemPalette !! c1
                    2 -> systemPalette !! c2
                    3 -> systemPalette !! c3
                    _ -> error "Bad color index"
                pixel = (color, if value == 0 then TransparentBG else OpaqueBG)
                pixelX = tileCol * 8 + x
                pixelY = tileRow * 8 + y
            whenR
                ( xStart_ vp <= pixelX
                    && pixelX < xEnd_ vp
                    && yStart_ vp <= pixelY
                    && pixelY < yEnd_ vp
                )
                $ withFrameState
                $ bufferSet pixel (pixelX + shiftX, pixelY + shiftY) . pixelBuffer

data ViewPort = MkVP {xEnd_ :: Int, yEnd_ :: Int, xStart_ :: Int, yStart_ :: Int}

getBackgroundPalette :: PPUPointers -> ByteString -> Int -> Int -> IO (Int, Int, Int, Int)
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
    Prelude.return (c0, c1, c2, c3)
