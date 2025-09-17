module Nes.CPU.State where

import Foreign
import Nes.Memory

-- | Offset in the vram of the next instruction to execute

-- | State of the CPU
data CPUState = MkCPUState
    { registerA :: {-# UNPACK #-} !Byte
    , registerX :: {-# UNPACK #-} !Byte
    , registerY :: {-# UNPACK #-} !Byte
    , status :: {-# UNPACK #-} !Byte
    , programCounter :: {-# UNPACK #-} !Addr
    }
    deriving (Eq, Show)

-- | Enumeration of the CPU's register
data Register = A | X | Y deriving (Eq, Show)

getRegisterPure :: Register -> CPUState -> Byte
getRegisterPure = \case
    A -> registerA
    X -> registerX
    Y -> registerY

setRegisterPure :: Register -> Byte -> CPUState -> CPUState
setRegisterPure reg byte st = case reg of
    A -> st{registerA = byte}
    X -> st{registerX = byte}
    Y -> st{registerY = byte}

-- | Get a brand new, clear CPU
--
-- Note: the PC will have to be set by reading value at 'programLocation'
newCPUState :: CPUState
newCPUState =
    MkCPUState
        { registerA = 0
        , registerX = 0
        , registerY = 0
        , -- see https://www.nesdev.org/wiki/Status_flags
          status = setBit 0 5
        , programCounter = 0
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
    deriving (Eq, Show)

setStatusFlagPure :: Flag -> CPUState -> CPUState
setStatusFlagPure flag st = st{status = setBit (status st) (unsafeFlagToBitOffset flag)}

clearStatusFlagPure :: Flag -> CPUState -> CPUState
clearStatusFlagPure flag st = st{status = clearBit (status st) (unsafeFlagToBitOffset flag)}

getStatusFlagPure :: Flag -> CPUState -> Bool
getStatusFlagPure flag st = testBit (status st) (unsafeFlagToBitOffset flag)

unsafeFlagToBitOffset :: Flag -> Int
unsafeFlagToBitOffset = \case
    Carry -> 0
    Zero -> 1
    InteruptDisable -> 2
    DecimalMode -> 3
    BreakCommand -> 4
    -- Note: No Bit 5
    -- https://www.nesdev.org/wiki/Status_flags
    Overflow -> 6
    Negative -> 7
