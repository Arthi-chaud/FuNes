module Nes.CPU.Instructions.LD (lda) where

import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.After (setZeroAndNegativeFlags)
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory

-- | Load Accumulator
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#LDA
lda :: AddressingMode -> CPU r ()
lda mode = do
    argAddr <- getOperandAddr mode
    param <- withBus $ readByte argAddr
    setRegister A param
    setZeroAndNegativeFlags param
