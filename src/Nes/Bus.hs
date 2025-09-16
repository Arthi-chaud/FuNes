module Nes.Bus where

import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.ForeignPtr (unsafeWithForeignPtr)
import Nes.Memory
import Nes.Memory.Unsafe ()

-- Constants

ramRange :: (MemoryAddr, MemoryAddr)
ramRange = (0x0000, 0x1fff)

ppuRegisters :: (MemoryAddr, MemoryAddr)
ppuRegisters = (0x2000, 0x3fff)

-- | The address where to read the program's offset
programLocation :: MemoryAddr
programLocation = 0xfffc

-- | End of the program's in the memory
programEnd :: MemoryAddr
programEnd = memorySize

-- | Interface for the CPU that allows it to read/write to RAM
newtype Bus = Bus {memory :: MemoryPointer}

instance MemoryInterface Bus where
    readByte idx (Bus fptr)
        | idx >= memorySize = fail "Out-of-bounds memory access"
        | otherwise = liftIO $ unsafeWithForeignPtr fptr (readByte idx)

    readAddr idx (Bus fptr) = liftIO $ unsafeWithForeignPtr fptr (readAddr idx)

-- | Translate a memory adress from vram to actual memory
translateMemoryAddr :: (MonadFail m) => MemoryAddr -> m MemoryAddr
translateMemoryAddr idx = return idx

-- TODO
-- \| inRange ramRange idx = return $ idx .&. 0b0000011111111111
-- \| inRange ppuRegisters idx =
--     let
--         _ = idx .&. 0b0010000000000111
--      in
--         fail "PPU is not supported yet" -- TODO
-- \| otherwise = fail $ printf "Invalid virtual memory access at 0x%x" idx
