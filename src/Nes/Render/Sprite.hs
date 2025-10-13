module Nes.Render.Sprite (renderSprites) where

import Control.Applicative
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

data Priority = Back | Front

-- | Renders sprites on frame
--
-- It  expects the background to be already drawn on the frame
renderSprites :: FrameBuffer -> Bus -> IO ()
renderSprites fb bus = do
    let oam = oamData $ ppuPointers bus
        chr = chrRom . cartridge $ bus
        bank = addrToInt . getSpritePatternAddr . controlRegister . ppuState $ bus
    sprites <- forM (reverse [0, 4 .. oamDataSize - 1]) $ \i -> do
        tileIdx <- byteToInt <$> readByte (fromIntegral i + 1) oam
        tileCol <- byteToInt <$> readByte (fromIntegral i + 3) oam
        tileRow <- byteToInt <$> readByte (fromIntegral i) oam
        attributes <- readByte (fromIntegral i + 2) oam
        let flipVertical = testBit attributes 7
            flipHorizontal = testBit attributes 6
            priority = if testBit attributes 5 then Back else Front
            paletteIdx = byteToInt (attributes .&. 0b11)
            tile =
                BS.take 16 $
                    BS.drop (bank + tileIdx * 16) chr
        (_, c1, c2, c3) <- getSpritePalette (ppuPointers bus) paletteIdx
        pixels <- fmap concat $ forM [0 .. 7] $ \y -> do
            let upper = BS.index tile y
                lower = BS.index tile (y + 8)
            forM (reverse [0 .. 7]) $ \x -> do
                let value =
                        ((1 .&. (lower `shiftR` (7 - x))) `shiftL` 1)
                            .|. (1 .&. (upper `shiftR` (7 - x)))
                let color = case value of
                        0 -> Nothing
                        1 -> Just $ systemPalette !! c1
                        2 -> Just $ systemPalette !! c2
                        3 -> Just $ systemPalette !! c3
                        _ -> Nothing
                return $
                    MkSP
                        ( flipCoord tileCol x flipHorizontal
                        , flipCoord tileRow y flipVertical
                        )
                        color

        return $ MkS{priority_ = priority, pixels_ = pixels}
    let pixels = toPixelsWithPriority sprites
    forM_ pixels $ \p -> case pixelColor p of
        Nothing -> pure ()
        Just c -> case pixelPriority p of
            Front -> frameBufferSetPixel (MkP Opaque c) (pixelCoord p) fb
            Back -> do
                backgroundPixel <- frameBufferGetPixel (pixelCoord p) fb
                case backgroundPixel of
                    MkP Transparent _ -> frameBufferSetPixel (MkP Opaque c) (pixelCoord p) fb
                    _ -> pure ()
  where
    flipCoord base n flip' = if not flip' then base + n else base + 7 - n

data Sprite = MkS {priority_ :: Priority, pixels_ :: [SpritePixel]}
data SpritePixel = MkSP PixelCoord (Maybe PixelColor)

-- | Sprites should be ordered back to front
toPixelsWithPriority :: [Sprite] -> [PixelWithPriority]
toPixelsWithPriority = go []
  where
    go :: [PixelWithPriority] -> [Sprite] -> [PixelWithPriority]
    go acc [] = acc
    go acc (s : ss) = go (mergeWithAcc (priority_ s) (pixels_ s) acc) ss

    toPixelWithPriority :: Priority -> SpritePixel -> PixelWithPriority
    toPixelWithPriority prio (MkSP coord color) = MkPC coord prio color

    mergeWithAcc :: Priority -> [SpritePixel] -> [PixelWithPriority] -> [PixelWithPriority]
    mergeWithAcc _ [] acc = acc
    mergeWithAcc prio pixels [] = toPixelWithPriority prio <$> pixels
    mergeWithAcc prio (p@(MkSP coord color) : pc) acc = mergeWithAcc prio pc $
        case span (\p' -> pixelCoord p' == coord) acc of
            (p' : _, acc') -> p'{pixelColor = color <|> pixelColor p'} : acc'
            ([], acc') -> toPixelWithPriority prio p : acc'

data PixelWithPriority = MkPC
    { pixelCoord :: PixelCoord
    , pixelPriority :: Priority
    , pixelColor :: Maybe PixelColor
    }

getSpritePalette :: PPUPointers -> Int -> IO (Int, Int, Int, Int)
getSpritePalette ptrs paletteIdx = do
    let offset = Addr $ fromIntegral $ 0x11 + (paletteIdx * 4)
    c1 <- byteToInt <$> readByte offset (paletteTable ptrs)
    c2 <- byteToInt <$> readByte (offset + 1) (paletteTable ptrs)
    c3 <- byteToInt <$> readByte (offset + 2) (paletteTable ptrs)
    return (0, c1, c2, c3)

-- TODO https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch6.4/src/render/mod.rs#L63
