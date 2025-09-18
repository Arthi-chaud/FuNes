module Nes.Bus where

import Nes.Memory
import Nes.Memory.Unsafe ()

-- Constants

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

-- | Interface for the CPU that allows it to read/write to RAM
newtype Bus = Bus {memory :: MemoryPointer}

instance MemoryInterface Bus where
    readByte idx (Bus fptr)
        | idx >= memorySize = fail "Out-of-bounds memory access"
        | otherwise = readByte idx fptr
    readAddr idx (Bus fptr) = readAddr idx fptr
    writeByte byte idx (Bus fptr) = writeByte byte idx fptr
    writeAddr addr idx (Bus fptr) = writeAddr addr idx fptr

-- | Translate a memory adress from vram to actual memory
translateAddr :: (MonadFail m) => Addr -> m Addr
translateAddr idx = return idx

-- TODO
-- \| inRange ramRange idx = return $ idx .&. 0b0000011111111111
-- \| inRange ppuRegisters idx =
--     let
--         _ = idx .&. 0b0010000000000111
--      in
--         fail "PPU is not supported yet" -- TODO
-- \| otherwise = fail $ printf "Invalid virtual memory access at 0x%x" idx
