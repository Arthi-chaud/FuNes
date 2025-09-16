module Nes.Memory (
    newMemory,
    memorySize,
    Byte,
    Addr,
    MemoryPointer,
    MemoryAddr,
    MemoryInterface (..),
) where

import Control.Monad.IO.Class
import Foreign

type Byte = Word8

type Addr = Word16

-- | The pointer used to interact with memory
type MemoryPointer = ForeignPtr ()

-- | Type of the offset from 'MemoryPointer' to interact with memory
type MemoryAddr = Word16

-- | The size of RAM, in bytes
--
-- Equivalent to 64 KiB
memorySize :: MemoryAddr
memorySize = 0xffff

-- | Creates a new memory slot
newMemory :: IO MemoryPointer
newMemory = mallocForeignPtrBytes $ fromIntegral memorySize

-- | Methods for interfaces that exposes memory
class MemoryInterface a where
    -- | Reads a single byte
    readByte :: (MonadIO m, MonadFail m) => MemoryAddr -> a -> m Byte

    -- | Reads two bytes packed in little endian
    readAddr :: (MonadIO m, MonadFail m) => MemoryAddr -> a -> m Addr

    -- | Writes a single byte
    writeByte :: (MonadIO m, MonadFail m) => Byte -> MemoryAddr -> a -> m ()

    -- | Writes two bytes packed in little endian
    writeAddr :: (MonadIO m, MonadFail m) => Addr -> MemoryAddr -> a -> m ()

-- TODO Write
