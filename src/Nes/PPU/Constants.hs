module Nes.PPU.Constants (
    vramSize,
    vramRange,
    paletteTableSize,
    paletteTableRange,
    paletteIndexes,
    oamDataSize,
    unusedAddrRange,
    chrRomSize,
    chrRomRange,
) where

import Nes.Memory

vramSize :: Int
vramSize = 2048

vramRange :: (Addr, Addr)
vramRange = (0x2000, 0x2fff)

paletteTableSize :: Int
paletteTableSize = 32

paletteIndexes :: [Addr]
paletteIndexes = [0x3f10, 0x3f14, 0x3f18, 0x3f1c]

paletteTableRange :: (Addr, Addr)
paletteTableRange = (0x3f00, 0x3fff)

oamDataSize :: Int
oamDataSize = 64 * 4

unusedAddrRange :: (Addr, Addr)
unusedAddrRange = (0x3000, 0x3eff)

chrRomRange :: (Addr, Addr)
chrRomRange = (0, 0x1fff)

chrRomSize :: Int
chrRomSize = 0x1fff
