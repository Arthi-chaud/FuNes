module Nes.Render.Sprite (renderSprites, applySprites) where

import Data.Bits
import qualified Data.ByteString as BS
import Nes.Bus
import Nes.Memory
import Nes.PPU.Constants
import Nes.PPU.Pointers
import Nes.PPU.State hiding (ScrollRegister (..))
import Nes.Render.Frame2
import Nes.Render.Monad
import qualified Nes.Render.Monad as Render
import Nes.Render.Palette
import Nes.Rom

-- | Renders sprites on buffer
--
-- It  expects the background to be already drawn on the pixel buffer
renderSprites :: Bus -> Render BGDrawn BGAndSpritesDrawn r ()
renderSprites bus = Render.do
    let oam = oamData $ ppuPointers bus
        chr = chrRom . cartridge $ bus
        bank = addrToInt . getSpritePatternAddr . controlRegister . ppuState $ bus
    forR (reverse [0, 4 .. oamDataSize - 1]) $ \i -> Render.do
        tileIdx <- liftIO $ byteToInt <$> readByte (fromIntegral i + 1) oam
        tileCol <- liftIO $ byteToInt <$> readByte (fromIntegral i + 3) oam
        tileRow <- liftIO $ byteToInt <$> readByte (fromIntegral i) oam
        attributes <- liftIO $ readByte (fromIntegral i + 2) oam
        let flipVertical = testBit attributes 7
            flipHorizontal = testBit attributes 6
            priority = if testBit attributes 5 then Back else Front
            paletteIdx = byteToInt (attributes .&. 0b11)
            tile =
                BS.take 16 $
                    BS.drop (bank + tileIdx * 16) chr
        (_, c1, c2, c3) <- liftIO $ getSpritePalette (ppuPointers bus) paletteIdx
        forR [0 .. 7] $ \y -> Render.do
            let upper = BS.index tile y
                lower = BS.index tile (y + 8)
            forR (reverse [0 .. 7]) $ \x -> Render.do
                let value =
                        ((1 .&. (lower `shiftR` (7 - x))) `shiftL` 1)
                            .|. (1 .&. (upper `shiftR` (7 - x)))
                let color = case value of
                        0 -> Nothing
                        1 -> Just $ systemPalette !! c1
                        2 -> Just $ systemPalette !! c2
                        3 -> Just $ systemPalette !! c3
                        _ -> Nothing
                    coord =
                        ( flipCoord tileCol x flipHorizontal
                        , flipCoord tileRow y flipVertical
                        )
                case (color, priority) of
                    (Nothing, _) -> Render.return ()
                    (Just c, prio) -> withFrameState $ bufferSet (Just (c, prio)) coord . spriteBuffer

    unsafeStep
  where
    flipCoord base n flip' = if not flip' then base + n else base + 7 - n

getSpritePalette :: PPUPointers -> Int -> IO (Int, Int, Int, Int)
getSpritePalette ptrs paletteIdx = do
    let offset = Addr $ fromIntegral $ 0x11 + (paletteIdx * 4)
    c1 <- byteToInt <$> readByte offset (paletteTable ptrs)
    c2 <- byteToInt <$> readByte (offset + 1) (paletteTable ptrs)
    c3 <- byteToInt <$> readByte (offset + 2) (paletteTable ptrs)
    Prelude.return (0, c1, c2, c3)

-- TODO https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch6.4/src/render/mod.rs#L63

-- | Merges the sprite buffer with the pixel buffer, using the priority atteched to pixels
applySprites :: Render BGAndSpritesDrawn Renderable r ()
applySprites = Render.do
    forR [0 .. bufferLength - 1] $ \i -> Render.do
        spritePixel <- withFrameState $ bufferGetOffset i . spriteBuffer
        bgPixel <- withFrameState $ bufferGetOffset i . pixelBuffer
        case (spritePixel, bgPixel) of
            (Nothing, _) -> Render.return ()
            (Just (c, Back), (_, TransparentBG)) -> withFrameState $ bufferSetOffset (c, Sprite) i . pixelBuffer
            (Just (_, Back), _) -> Render.return ()
            (Just (c, Front), _) -> withFrameState $ bufferSetOffset (c, Sprite) i . pixelBuffer
    unsafeStep
