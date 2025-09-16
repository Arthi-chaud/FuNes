module Nes.CPU.Instructions.TA (tax) where

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
