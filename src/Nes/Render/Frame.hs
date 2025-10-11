module Nes.Render.Frame (
    -- * Type
    Frame (..),

    -- * Functions
    newFrame,
    frameSetPixel,

    -- * Dimensions
    frameWidth,
    frameHeight,
    frameLength,
) where

import Control.Monad
import Data.Ix
import Data.Word
import Foreign
import GHC.ForeignPtr
import Nes.Internal
import Nes.Memory

newtype Frame = MkF {unF :: MemoryPointer}

frameWidth :: Int
frameWidth = 256

frameHeight :: Int
frameHeight = 240

frameLength :: Int
frameLength = frameWidth * frameHeight * 3

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
