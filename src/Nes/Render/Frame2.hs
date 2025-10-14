{-# LANGUAGE RecordWildCards #-}

module Nes.Render.Frame2 (
    -- * State
    FrameState (..),
    newFrameState,

    -- * Frame
    frameWidth,
    frameHeight,
    frameLength,
    frameSetPixel,

    -- * Buffer
    Buffer (..),
    bufferWidth,
    bufferHeight,
    bufferLength,
    bufferGet,
    bufferSet,
    bufferCoordToOffset,
    bufferGetOffset,
    bufferSetOffset,

    -- * Pixel
    Coord,
    Color,
    PixelType (..),

    -- * Misc
    SpritePriority (..),
) where

import Control.Monad
import Data.Array.IO
import Foreign hiding (newArray)
import GHC.ForeignPtr
import Nes.Internal
import Nes.Memory (MemoryPointer)

-- | Width (in pixels of the frame)
frameWidth :: Int
frameWidth = 256

-- | Height (in pixels of the frame)
frameHeight :: Int
frameHeight = 240

-- | Size, in bytes, of the frame
frameLength :: Int
frameLength = frameWidth * frameHeight * 3

frameSetPixel :: (Word8, Word8, Word8) -> (Int, Int) -> MemoryPointer -> IO ()
frameSetPixel (colorR, colorG, colorB) (x, y) fptr = do
    let base = y * 3 * frameWidth + x * 3
    when (inRange (0, frameLength) (base + 2)) $ do
        -- Note: we don't use 'writeByte' because base + 2 might overflow
        unsafeWithForeignPtr (castForeignPtr fptr) $ \ptr -> do
            pokeByteOff ptr base colorR
            pokeByteOff ptr (base + 1) colorG
            pokeByteOff ptr (base + 2) colorB

-- | Width of the internal buffer
--
-- Equals 'frameWidth'
bufferWidth :: Int
bufferWidth = frameWidth

-- | Height of the internal buffer
--
-- _Warning_ is not equal to 'frameHeight'
--
-- See https://www.nesdev.org/wiki/PPU_rendering#Vertical_blanking_lines_(241-260)
bufferHeight :: Int
bufferHeight = 260

-- | Number of elements in a buffer
bufferLength :: Int
bufferLength = bufferWidth * bufferHeight

bufferSet :: a -> Coord -> Buffer a -> IO ()
bufferSet pixel coord = bufferSetOffset pixel (bufferCoordToOffset coord)

bufferSetOffset :: a -> Int -> Buffer a -> IO ()
bufferSetOffset pixel offset (MkBuffer fb) =
    writeArray
        fb
        offset
        pixel

bufferGet :: Coord -> Buffer a -> IO a
bufferGet coord (MkBuffer fb) = readArray fb (bufferCoordToOffset coord)

bufferGetOffset :: Int -> Buffer a -> IO a
bufferGetOffset offset (MkBuffer fb) = readArray fb offset

bufferCoordToOffset :: Coord -> Int
bufferCoordToOffset (x, y) = y * frameWidth + x

data FrameState = MkFrameState
    { sdl2Frame :: {-# UNPACK #-} !MemoryPointer
    , pixelBuffer :: {-# UNPACK #-} !(Buffer (Color, PixelType))
    , spriteBuffer :: {-# UNPACK #-} !(Buffer (Maybe (Color, SpritePriority)))
    }

-- | Allocates the buffers and frame
newFrameState :: IO FrameState
newFrameState = do
    !sdl2Frame <- callocForeignPtr frameLength
    !pixelBuffer <- MkBuffer <$> newArray (0, bufferLength) ((0, 0, 0), TransparentBG)
    !spriteBuffer <- MkBuffer <$> newArray (0, bufferLength) Nothing
    return $ MkFrameState{..}

newtype Buffer a = MkBuffer (IOArray Int a)

-- | Says _where_ the pixel comes from
data PixelType
    = -- | An opaque background pixel (e.g. pipe in mario)
      TransparentBG
    | -- | A transparent background pixel (e.g. sky in mario)
      OpaqueBG
    | -- | Pixel from a sprite
      Sprite
    deriving (Eq, Show)

-- | Priority of the sprite (from bit 5 of attribute table)
--
-- https://www.nesdev.org/wiki/PPU_sprite_priority
data SpritePriority = Front | Back deriving (Eq, Show)

type Color = (Word8, Word8, Word8)

type Coord = (Int, Int)
