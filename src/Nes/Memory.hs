{-# LANGUAGE MultiParamTypeClasses #-}

module Nes.Memory (
    newMemory,
    memorySize,
    Byte (..),
    Addr (..),
    byteToAddr,
    bytesToAddr,
    byteToInt,
    addrToInt,
    unsafeAddrToByte,
    MemoryPointer,
    MemoryInterface (..),
) where

import Data.Ix (Ix)
import Foreign

newtype Byte = Byte {unByte :: Word8} deriving (Eq, Show, Num, Bits, Ord)

newtype Addr = Addr {unAddr :: Word16} deriving (Eq, Show, Num, Bits, Ord, Ix)

byteToAddr :: Byte -> Addr
byteToAddr = Addr . fromIntegral . unByte

byteToInt :: Byte -> Int
byteToInt = fromIntegral . unByte

addrToInt :: Addr -> Int
addrToInt = fromIntegral . unAddr

-- | Unsafe: Casts a Word16 to Word8
unsafeAddrToByte :: Addr -> Byte
unsafeAddrToByte (Addr a) = Byte $ fromIntegral a

-- | First byte is low-end, second is high-end
bytesToAddr :: Byte -> Byte -> Addr
bytesToAddr low high = shiftL (byteToAddr high) 8 .|. byteToAddr low

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
class MemoryInterface a m where
    -- | Reads a single byte
    readByte :: Addr -> a -> m Byte

    -- | Reads two bytes packed in little endian
    readAddr :: Addr -> a -> m Addr

    -- | Writes a single byte
    writeByte :: Byte -> Addr -> a -> m ()

    -- | Writes two bytes packed in little endian
    --
    -- the first argument is the address to write
    -- the second is the destination
    writeAddr :: Addr -> Addr -> a -> m ()
