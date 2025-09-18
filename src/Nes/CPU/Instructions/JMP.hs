module Nes.CPU.Instructions.JMP (jmp) where

import Control.Monad
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Monad

-- | Sets the program counter to the address specified by the operand
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP
jmp :: AddressingMode -> CPU r ()
jmp = getOperandAddr >=> setPC
