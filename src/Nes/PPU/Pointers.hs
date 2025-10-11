{-# LANGUAGE RecordWildCards #-}

module Nes.PPU.Pointers (
    PPUPointers (chrRom, paletteTable, vram, oamData),
    newPPUPointers,
) where

import Data.ByteString
import Nes.Internal
import Nes.Memory
import Nes.PPU.Constants

data PPUPointers = MkPPUPtr
    { chrRom :: {-# UNPACK #-} !ByteString
    , paletteTable :: {-# UNPACK #-} !MemoryPointer
    , vram :: {-# UNPACK #-} !MemoryPointer
    , oamData :: {-# UNPACK #-} !MemoryPointer
    }

newPPUPointers :: ByteString -> IO PPUPointers
newPPUPointers chrRom = do
    paletteTable <- callocForeignPtr paletteTableSize
    vram <- callocForeignPtr vramSize
    oamData <- callocForeignPtr oamDataSize
    return MkPPUPtr{..}
