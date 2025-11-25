{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Nes.CPU.State (
    -- * State
    CPUState (..),
    newCPUState,

    -- * Accessing registers
    Register (..),
    register,

    -- * Accessing status flags
    StatusRegister (..),
    StatusRegisterFlag (..),

    -- * Lenses
    registerA,
    registerX,
    registerY,
    registerS,
    status,
    pc,
) where

import Control.Lens (Lens', makeLenses)
import Nes.Bus.Constants (stackReset)
import Nes.FlagRegister
import Nes.Memory

-- | Offset in the vram of the next instruction to execute

-- | State of the CPU
data CPUState = MkCPUState
    { _registerA :: {-# UNPACK #-} !Byte
    -- ^ Aka Accumulator
    , _registerX :: {-# UNPACK #-} !Byte
    , _registerY :: {-# UNPACK #-} !Byte
    , _registerS :: {-# UNPACK #-} !Byte
    -- ^ Aka Stack pointer
    , _status :: {-# UNPACK #-} !StatusRegister
    , _pc :: {-# UNPACK #-} !Addr
    -- ^ Program counter
    }
    deriving (Eq, Show)

newtype StatusRegister = MkSR {unSR :: Byte} deriving (Eq, Show)

makeLenses ''CPUState

-- | Get a brand new, clear CPU
--
-- Note: the PC will have to be set by reading value at 'programLocation'
newCPUState :: CPUState
newCPUState =
    MkCPUState
        { _registerA = 0
        , _registerX = 0
        , _registerY = 0
        , _registerS = stackReset
        , -- see https://www.nesdev.org/wiki/Status_flags
          -- and https://bugzmanov.github.io/nes_ebook/chapter_4.html
          _status = MkSR 0b00100100
        , _pc = 0
        }

-- | Enumeration of the CPU's registers
data Register = A | X | Y | S deriving (Eq, Show)

register :: Register -> Lens' CPUState Byte
register = \case
    A -> registerA
    X -> registerX
    Y -> registerY
    S -> registerS

-- | Flags for the CPU's status
--
-- https://www.nesdev.org/obelisk-6502-guide/registers.html#C
data StatusRegisterFlag
    = Carry
    | Zero
    | InterruptDisable
    | DecimalMode
    | BFlag
    | Unusued
    | Overflow
    | Negative
    deriving (Eq, Show, Enum)

instance FlagRegister StatusRegister where
    type Flag StatusRegister = StatusRegisterFlag
    fromByte = MkSR
    toByte = unSR
    flagToBitOffset = fromEnum
