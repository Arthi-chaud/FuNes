module Nes.Render.Frame (
    -- * Frame
    Frame (..),
    newFrame,
    frameSetPixel,
    frameToByteString,

    -- * Frame Buffer
    FrameBuffer (..),
    FrameBufferCoord,
    FrameBufferPixel (..),
    newFrameBuffer,
    frameBufferSetPixel,
    renderFrameBuffer,

    -- * Dimensions
    frameWidth,
    frameHeight,
) where

import Control.Monad
import Data.Array
import Data.Array.Base
import Data.Array.IO (IOArray)
import qualified Data.Array.MArray
import Data.ByteString.Internal
import Data.Word
import Foreign
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

type FrameBufferCoord = (Int, Int)

data FrameBufferPixel = Transparent | Opaque Word8 Word8 Word8

newtype FrameBuffer = MkFB {unFB :: IOArray FrameBufferCoord FrameBufferPixel}

newFrameBuffer :: IO FrameBuffer
newFrameBuffer = MkFB <$> Data.Array.MArray.newArray ((0, 0), (frameWidth - 1, frameHeight - 1)) Transparent

frameBufferSetPixel :: (Word8, Word8, Word8) -> (Int, Int) -> FrameBuffer -> IO ()
frameBufferSetPixel (r, g, b) coord (MkFB fb) = writeArray fb coord $ Opaque r g b

renderFrameBuffer :: FrameBuffer -> Frame -> IO ()
renderFrameBuffer (MkFB fb) f =
    getAssocs fb
        >>= mapM_
            ( \(coord, pixel) ->
                case pixel of
                    Transparent -> pure ()
                    Opaque r g b -> frameSetPixel (r, g, b) coord f
            )
