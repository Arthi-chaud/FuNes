module Nes.CPU.State (
    -- * State
    CPUState (..),
    newCPUState,

    -- * Accessing registers
    Register (..),
    getRegister,
    setRegister,
    modifyStatusRegister,

    -- * Accessing status flags
    StatusRegister (..),
    StatusRegisterFlag (..),
) where

import Nes.Bus.Constants (stackReset)
import Nes.FlagRegister
import Nes.Memory

-- | Offset in the vram of the next instruction to execute

-- | State of the CPU
data CPUState = MkCPUState
    { registerA :: {-# UNPACK #-} !Byte
    -- ^ Aka Accumulator
    , registerX :: {-# UNPACK #-} !Byte
    , registerY :: {-# UNPACK #-} !Byte
    , registerS :: {-# UNPACK #-} !Byte
    -- ^ Aka Stack pointer
    , status :: {-# UNPACK #-} !StatusRegister
    , programCounter :: {-# UNPACK #-} !Addr
    }
    deriving (Eq, Show)

-- | Enumeration of the CPU's registers
data Register = A | X | Y | S deriving (Eq, Show)

getRegister :: Register -> CPUState -> Byte
getRegister = \case
    A -> registerA
    X -> registerX
    Y -> registerY
    S -> registerS

setRegister :: Register -> Byte -> CPUState -> CPUState
setRegister reg byte st = case reg of
    A -> st{registerA = byte}
    X -> st{registerX = byte}
    Y -> st{registerY = byte}
    S -> st{registerS = byte}

-- | Get a brand new, clear CPU
--
-- Note: the PC will have to be set by reading value at 'programLocation'
newCPUState :: CPUState
newCPUState =
    MkCPUState
        { registerA = 0
        , registerX = 0
        , registerY = 0
        , registerS = stackReset
        , -- see https://www.nesdev.org/wiki/Status_flags
          -- and https://bugzmanov.github.io/nes_ebook/chapter_4.html
          status = MkSR 0b00100100
        , programCounter = 0
        }

newtype StatusRegister = MkSR {unSR :: Byte} deriving (Eq, Show)

-- | Flags for the CPU's status
--
-- https://www.nesdev.org/obelisk-6502-guide/registers.html#C
data StatusRegisterFlag
    = Carry
    | Zero
    | InterruptDisable
    | DecimalMode
    | BreakCommand
    | BreakCommand2
    | Overflow
    | Negative
    deriving (Eq, Show, Enum)

instance FlagRegister StatusRegister where
    type Flag StatusRegister = StatusRegisterFlag
    fromByte = MkSR
    toByte = unSR
    flagToBitOffset = fromEnum

modifyStatusRegister :: (StatusRegister -> StatusRegister) -> CPUState -> CPUState
modifyStatusRegister f st = st{status = f $ status st}
