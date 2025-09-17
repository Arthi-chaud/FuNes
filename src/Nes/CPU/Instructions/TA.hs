module Nes.CPU.Instructions.TA (tax, tay) where

import Nes.CPU.Instructions.After
import Nes.CPU.Monad
import Nes.CPU.State

-- | Transfer Accumulator to X
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TAX
tax :: CPU r ()
tax = do
    regA <- getRegister A
    setRegister X regA
    setZeroAndNegativeFlags regA

-- | Transfer Accumulator to Y
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TAY
tay :: CPU r ()
tay = do
    regA <- getRegister A
    setRegister Y regA
    setZeroAndNegativeFlags regA
