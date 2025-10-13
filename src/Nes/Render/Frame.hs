module Nes.Render.Frame (
    -- * Frame
    Frame (..),
    newFrame,
    frameSetPixel,
    frameToByteString,

    -- * Frame Buffer
    FrameBuffer (..),
    FrameBufferPixel (..),
    FrameBufferPixelType (..),
    newFrameBuffer,
    frameBufferSetPixel,
    frameBufferGetPixel,
    renderFrameBuffer,

    -- * Pixel
    PixelCoord,
    PixelColor,

    -- * Dimensions
    frameWidth,
    frameHeight,
) where

import Control.Monad
import Data.Array
import Data.Array.Base
import Data.Array.IO (IOArray)
import Data.ByteString.Internal
import Data.Word
import Foreign (castForeignPtr, pokeByteOff)
import Nes.Internal
import Nes.Memory

-- | Frame buffer for SDL2
newtype Frame = MkF {unF :: MemoryPointer}

frameWidth :: Int
frameWidth = 256

frameHeight :: Int
frameHeight = 240

frameLength :: Int
frameLength = frameWidth * frameHeight * 3

frameToByteString :: Frame -> ByteString
frameToByteString (MkF fptr) = BS (castForeignPtr fptr) frameLength

newFrame :: IO Frame
newFrame = MkF <$> callocForeignPtr frameLength

frameSetPixel :: (Word8, Word8, Word8) -> (Int, Int) -> Frame -> IO ()
frameSetPixel (colorR, colorG, colorB) (x, y) (MkF fptr) = do
    let base = y * 3 * frameWidth + x * 3
    when (inRange (0, frameLength) (base + 2)) $ do
        -- Note: we don't use 'writeByte' because base + 2 might overflow
        unsafeWithForeignPtr (castForeignPtr fptr) $ \ptr -> do
            pokeByteOff ptr base colorR
            pokeByteOff ptr (base + 1) colorG
            pokeByteOff ptr (base + 2) colorB

type PixelCoord = (Int, Int)
type PixelColor = (Word8, Word8, Word8)

data FrameBufferPixel = MkP
    { type_ :: FrameBufferPixelType
    , color_ :: PixelColor
    }

data FrameBufferPixelType = Transparent | Opaque

newtype FrameBuffer = MkFB {unFB :: IOArray Int FrameBufferPixel}

frameBufferHeight :: Int
frameBufferHeight = 262

pixelCoordToFrameBufferOffset :: PixelCoord -> Int
pixelCoordToFrameBufferOffset (x, y) = y * frameWidth + x

newFrameBuffer :: IO FrameBuffer
newFrameBuffer = MkFB <$> newArray (0, frameBufferHeight * frameWidth) (MkP Transparent (0, 0, 0))

frameBufferSetPixel :: FrameBufferPixel -> (Int, Int) -> FrameBuffer -> IO ()
frameBufferSetPixel pixel coord (MkFB fb) = do
    writeArray
        fb
        (pixelCoordToFrameBufferOffset coord)
        pixel

frameBufferGetPixel :: (Int, Int) -> FrameBuffer -> IO FrameBufferPixel
frameBufferGetPixel coord (MkFB fb) = readArray fb (pixelCoordToFrameBufferOffset coord)

renderFrameBuffer :: FrameBuffer -> Frame -> IO ()
renderFrameBuffer (MkFB fb) f = forM_ [0 .. frameWidth * frameHeight - 1] $ \offset -> do
    let coord = (offset `mod` frameWidth, offset `div` frameWidth)
    pixel <- readArray fb offset
    frameSetPixel (color_ pixel) coord f
