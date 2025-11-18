module Nes.Interrupt (
    -- * Interrupt Enum
    Interrupt (..),
    IRQSource (..),
    getVectorAddr,
    pushesBFlag,

    -- * Status
    InterruptStatus (..),
    modifyPendingInterrupt,
    popInterrupt,
    pushInterrupt,
) where

import Data.Maybe (listToMaybe)
import Nes.Memory

data Interrupt = NMI | IRQ IRQSource deriving (Eq, Show)

data IRQSource = BRK | DMC | FrameCounter deriving (Eq, Show)

getVectorAddr :: Interrupt -> Addr
getVectorAddr = \case
    NMI -> 0xfffa
    IRQ _ -> 0xfffe

pushesBFlag :: Interrupt -> Bool
pushesBFlag = \case
    NMI -> False
    IRQ BRK -> True
    IRQ _ -> False

data InterruptStatus = MkIE
    { pendingInterrupts :: {-# UNPACK #-} ![Interrupt]
    -- ^ Will be true if the CPU is executing the interrupt handler
    }

modifyPendingInterrupt :: ([Interrupt] -> [Interrupt]) -> InterruptStatus -> InterruptStatus
modifyPendingInterrupt f s = s{pendingInterrupts = f $ pendingInterrupts s}

pushInterrupt :: Interrupt -> InterruptStatus -> InterruptStatus
pushInterrupt i = modifyPendingInterrupt (++ [i])

popInterrupt :: InterruptStatus -> (Maybe Interrupt, InterruptStatus)
popInterrupt st =
    let
        pendingHead = take 1 $ pendingInterrupts st
        st1 = modifyPendingInterrupt (drop 1) st
     in
        (listToMaybe pendingHead, st1)
