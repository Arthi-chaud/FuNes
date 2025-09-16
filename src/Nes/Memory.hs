module Nes.Memory (
    newMemory,
    memorySize,
    Byte (..),
    Addr (..),
    byteToAddr,
    byteToInt,
    addrToInt,
    MemoryPointer,
    MemoryInterface (..),
) where

import Control.Monad.IO.Class
import Foreign

newtype Byte = Byte {unByte :: Word8} deriving (Eq, Show, Num, Bits, Ord)

newtype Addr = Addr {unAddr :: Word16} deriving (Eq, Show, Num, Bits, Ord)

byteToAddr :: Byte -> Addr
byteToAddr = Addr . fromIntegral . unByte

byteToInt :: Byte -> Int
byteToInt = fromIntegral . unByte

addrToInt :: Addr -> Int
addrToInt = fromIntegral . unAddr

-- | The pointer used to interact with memory
type MemoryPointer = ForeignPtr ()

-- | The size of RAM, in bytes
--
-- Equivalent to 64 KiB
memorySize :: Addr
memorySize = 0xffff

-- | Creates a new memory slot
newMemory :: IO MemoryPointer
newMemory = mallocForeignPtrBytes $ fromIntegral $ unAddr memorySize

-- | Methods for interfaces that exposes memory
class MemoryInterface a where
    -- | Reads a single byte
    readByte :: (MonadIO m, MonadFail m) => Addr -> a -> m Byte

    -- | Reads two bytes packed in little endian
    readAddr :: (MonadIO m, MonadFail m) => Addr -> a -> m Addr

    -- | Writes a single byte
    writeByte :: (MonadIO m, MonadFail m) => Byte -> Addr -> a -> m ()

    -- | Writes two bytes packed in little endian
    writeAddr :: (MonadIO m, MonadFail m) => Addr -> Addr -> a -> m ()

-- TODO Write
