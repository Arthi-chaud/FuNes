{-# LANGUAGE RecordWildCards #-}

module Nes.PPU.Pointers (
    PPUPointers (paletteTable, vram, oamData),
    newPPUPointers,
) where

import Nes.Internal
import Nes.Memory
import Nes.PPU.Constants

data PPUPointers = MkPPUPtr
    { paletteTable :: {-# UNPACK #-} !MemoryPointer
    , vram :: {-# UNPACK #-} !MemoryPointer
    , oamData :: {-# UNPACK #-} !MemoryPointer
    }

newPPUPointers :: IO PPUPointers
newPPUPointers = do
    paletteTable <- callocForeignPtr paletteTableSize
    vram <- callocForeignPtr vramSize
    oamData <- callocForeignPtr oamDataSize
    return MkPPUPtr{..}
