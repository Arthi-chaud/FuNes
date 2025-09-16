module Nes.CPU.State where

import Foreign
import Nes.Bus (programLocation)
import Nes.Memory

-- | Offset in the vram of the next instruction to execute
newtype ProgramCounter = PC {unPC :: MemoryAddr} deriving (Eq, Show, Num)

-- | State of the CPU
data CPUState = MkCPUState
    { registerA :: {-# UNPACK #-} !Word8
    , registerX :: {-# UNPACK #-} !Word8
    , status :: {-# UNPACK #-} !Word8
    , programCounter :: {-# UNPACK #-} !ProgramCounter
    }
    deriving (Eq, Show)

-- | Enumeration of the CPU's register
data Register = A | X deriving (Eq, Show)

getRegisterPure :: Register -> CPUState -> Word8
getRegisterPure = \case
    A -> registerA
    X -> registerX

setRegisterPure :: Register -> Word8 -> CPUState -> CPUState
setRegisterPure reg byte st = case reg of
    A -> st{registerA = byte}
    X -> st{registerX = byte}

-- | Get a brand new, clear CPU
newCPUState :: CPUState
newCPUState =
    MkCPUState
        { registerA = 0
        , registerX = 0
        , status = 0
        , programCounter = PC programLocation
        }

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
