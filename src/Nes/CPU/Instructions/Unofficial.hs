module Nes.CPU.Instructions.Unofficial (lax) where

import Nes.CPU.Instructions.Addressing
import Nes.CPU.Monad
import Nes.CPU.State (Register (..))
import Nes.Memory

-- | Equivalent to LDA then TAX, saves a couple of cycles
--
-- https://www.nesdev.org/wiki/Programming_with_unofficial_opcodes
lax :: AddressingMode -> CPU r ()
lax mode = do
    addr <- getOperandAddr mode
    byte <- readByte addr ()
    setRegister A byte
    setRegister X byte
