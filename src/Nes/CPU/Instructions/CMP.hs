module Nes.CPU.Instructions.CMP (cmp) where

import Control.Monad
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.After
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory

-- | Computes (Register A - _value in memory_)
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CMP
cmp :: AddressingMode -> CPU r ()
cmp mode = do
    value <- getOperandAddr mode >>= withBus . readByte
    regA <- getRegister A
    let diff = regA - value
    setZeroAndNegativeFlags diff
    when (regA >= value) $ setStatusFlag Carry
