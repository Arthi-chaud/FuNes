module HNes.CPU.State where

import Foreign
import HNes.Memory

-- | Offset in the vram of the next instruction to execute
newtype ProgramCounter = PC {unPC :: MemoryAddr} deriving (Eq, Show, Num)

-- | State of the CPU
data CPUState = MkCPUState
    { registerA :: {-# UNPACK #-} !Word8
    , status :: {-# UNPACK #-} !Word8
    , programCounter :: {-# UNPACK #-} !ProgramCounter
    }
    deriving (Eq, Show)

-- | Get a brand new, clear CPU
newCPUState :: CPUState
newCPUState = MkCPUState 0 0 (PC 0)

-- | Flags for the CPU's status
--
-- https://www.nesdev.org/obelisk-6502-guide/registers.html#C
data Flag
    = Carry
    | Zero
    | InteruptDisable
    | DecimalMode
    | BreakCommand
    | Overflow
    | Negative

setStatusFlagPure :: Flag -> CPUState -> CPUState
setStatusFlagPure flag st = st{status = setBit (status st) (unsafeFlagToBitOffset flag)}

clearStatusFlagPure :: Flag -> CPUState -> CPUState
clearStatusFlagPure flag st = st{status = clearBit (status st) (unsafeFlagToBitOffset flag)}

getStatusFlagPure :: Flag -> CPUState -> Bool
getStatusFlagPure flag st = testBit (status st) (unsafeFlagToBitOffset flag)

unsafeFlagToBitOffset :: Flag -> Int
unsafeFlagToBitOffset = \case
    Zero -> 1
    Negative -> 7
    _ -> error "TODO"
