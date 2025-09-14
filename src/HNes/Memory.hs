module HNes.Memory (newMemory, memorySize, MemoryPointer, MemoryInterface (..)) where

import Foreign

type MemoryPointer = ForeignPtr ()

-- | The size of RAM, in bytes
memorySize :: Word16
memorySize = 2048

-- | Creates a new memory slot
newMemory :: IO MemoryPointer
newMemory = mallocForeignPtrBytes $ fromIntegral memorySize

-- | Methods for interfaces that exposes memory
class MemoryInterface a where
    readWord8 :: Word16 -> a -> IO Word8
