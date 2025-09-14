module HNes.CPU.Instructions.LDA (lda) where

import Data.Bits
import HNes.CPU.Monad
import HNes.CPU.State

-- | Load Accumulator
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#LDA
lda :: CPU r ()
lda = do
    param <- readAtPC
    incrementPC
    setRegisterA param
    after

after :: CPU r ()
after = do
    regA <- getRegisterA
    if regA == 0 then setStatusFlag Zero else clearStatusFlag Zero
    if testBit regA 7 then setStatusFlag Negative else clearStatusFlag Negative
