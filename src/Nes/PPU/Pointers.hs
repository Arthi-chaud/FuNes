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
    { chrRom :: ByteString
    , paletteTable :: MemoryPointer
    , vram :: MemoryPointer
    , oamData :: MemoryPointer
    }

newPPUPointers :: ByteString -> IO PPUPointers
newPPUPointers chrRom = do
    paletteTable <- callocForeignPtr paletteTableSize
    vram <- callocForeignPtr vramSize
    oamData <- callocForeignPtr oamDataSize
    return MkPPUPtr{..}
