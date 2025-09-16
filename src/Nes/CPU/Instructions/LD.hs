module Nes.CPU.Instructions.LD (lda) where

import Nes.CPU.Instructions.After (setZeroAndNegativeFlags)
import Nes.CPU.Monad
import Nes.CPU.State

-- | Load Accumulator
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#LDA
lda :: CPU r ()
lda = do
    param <- readAtPC
    incrementPC
    setRegister A param
    setZeroAndNegativeFlags param
