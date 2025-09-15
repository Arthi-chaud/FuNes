module HNes.CPU.Instructions.LD (lda) where

import HNes.CPU.Instructions.After (setZeroAndNegativeFlags)
import HNes.CPU.Monad
import HNes.CPU.State

-- | Load Accumulator
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#LDA
lda :: CPU r ()
lda = do
    param <- readAtPC
    incrementPC
    setRegister A param
    setZeroAndNegativeFlags param
