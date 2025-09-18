module Nes.CPU.Instructions.CMP (cmp, cpx, cpy) where

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
cmp = compareWithRegister A

-- | Computes (Register X - _value in memory_)
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CPX
cpx :: AddressingMode -> CPU r ()
cpx = compareWithRegister X

-- | Computes (Register Y - _value in memory_)
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CPY
cpy :: AddressingMode -> CPU r ()
cpy = compareWithRegister Y

compareWithRegister :: Register -> AddressingMode -> CPU r ()
compareWithRegister reg mode = do
    value <- getOperandAddr mode >>= withBus . readByte
    regValue <- getRegister reg
    let diff = regValue - value
    setZeroAndNegativeFlags diff
    when (regValue >= value) $ setStatusFlag Carry
