module HNes.CPU.Instructions.TA (tax) where

import HNes.CPU.Instructions.After
import HNes.CPU.Monad
import HNes.CPU.State

-- | Transfer Accumulator to X
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TAX
tax :: CPU r ()
tax = do
    regA <- getRegister A
    setRegister X regA
    setZeroAndNegativeFlags regA
