module Nes.Memory (newMemory, memorySize, MemoryPointer, MemoryAddr, MemoryInterface (..)) where

import Foreign

-- | The pointer used to interact with memory
type MemoryPointer = ForeignPtr ()

-- | Type of the offset from 'MemoryPointer' to interact with memory
type MemoryAddr = Word16

-- | The size of RAM, in bytes
memorySize :: MemoryAddr
memorySize = 2048

-- | Creates a new memory slot
newMemory :: IO MemoryPointer
newMemory = mallocForeignPtrBytes $ fromIntegral memorySize

-- | Methods for interfaces that exposes memory
class MemoryInterface a where
    readWord8 :: MemoryAddr -> a -> IO Word8

-- TODO Write
