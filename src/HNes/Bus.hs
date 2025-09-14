module HNes.Bus where

import Data.Ix
import Foreign
import GHC.ForeignPtr (unsafeWithForeignPtr)
import GHC.Storable (readWord8OffPtr)
import HNes.Memory
import Text.Printf

-- Constants

ramRange :: (Word16, Word16)
ramRange = (0x0000, 0x1fff)

ppuRegisters :: (Word16, Word16)
ppuRegisters = (0x2000, 0x3fff)

-- | Interface for the CPU that allows it to read/write to RAM
newtype Bus = Bus {memory :: MemoryPointer}

instance MemoryInterface Bus where
    readWord8 idx (Bus ptr)
        | idx >= memorySize = fail "Out-of-bounds memory access"
        | otherwise = do
            addr <- translateMemoryAddr idx
            unsafeWithForeignPtr ptr (flip readWord8OffPtr (fromIntegral addr) . castPtr)

-- | Translate a memory adress from vram to actual memory
translateMemoryAddr :: (MonadFail m) => Word16 -> m Word16
translateMemoryAddr idx
    | inRange ramRange idx = return $ idx .&. 0b0000011111111111
    | inRange ppuRegisters idx =
        let
            _ = idx .&. 0b0010000000000111
         in
            fail "PPU is not supported yet" -- TODO
    | otherwise = fail $ printf "Ignoring invalid virtual memory access at %d" idx
