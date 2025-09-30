module Nes.Bus.Constants (
    ramRange,
    stackAddr,
    stackReset,
    ppuRegisters,
    programLocation,
    programEnd,
    prgRomRange,
) where

import Nes.Memory

ramRange :: (Addr, Addr)
ramRange = (0x0000, 0x1fff)

stackAddr :: Addr
stackAddr = 0x0100

stackReset :: Byte
stackReset = 0xfd

ppuRegisters :: (Addr, Addr)
ppuRegisters = (0x2000, 0x3fff)

-- | The address where to read the program's offset
programLocation :: Addr
programLocation = 0xfffc

-- | End of the program's in the memory
programEnd :: Addr
programEnd = memorySize

prgRomRange :: (Addr, Addr)
prgRomRange = (0x8000, 0xffff)
