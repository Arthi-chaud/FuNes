module Nes.Interrupt (
    -- * Interrupt Enum
    Interrupt (..),
    IRQSource (..),
    getVectorAddr,
    getFlagMask,

    -- * Status
    InterruptStatus (..),
    modifyPendingInterrupt,
) where

import Nes.Memory

data Interrupt = NMI | BRK | IRQ IRQSource deriving (Eq, Show)

data IRQSource = DMA | FrameCounter deriving (Eq, Show)

getVectorAddr :: Interrupt -> Addr
getVectorAddr = \case
    NMI -> 0xfffa
    BRK -> 0xfffe
    IRQ _ -> 0xfffe

getFlagMask :: Interrupt -> Byte
getFlagMask = \case
    NMI -> 0b00100000
    IRQ _ -> 0b00100000
    BRK -> 0b00110000

data InterruptStatus = MkIE
    { pendingInterrupts :: {-# UNPACK #-} ![Interrupt]
    -- ^ Will be true if the CPU is executing the interrupt handler
    }

modifyPendingInterrupt :: ([Interrupt] -> [Interrupt]) -> InterruptStatus -> InterruptStatus
modifyPendingInterrupt f s = s{pendingInterrupts = f $ pendingInterrupts s}
