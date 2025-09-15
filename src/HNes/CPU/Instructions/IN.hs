module HNes.CPU.Instructions.IN (inx) where

import HNes.CPU.Instructions.After
import HNes.CPU.Monad
import HNes.CPU.State

-- | Increment the value of the X register X
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#INX
inx :: CPU r ()
inx = do
    newRegX <- (+ 1) <$> getRegister X
    setRegister X newRegX
    setZeroAndNegativeFlags newRegX
