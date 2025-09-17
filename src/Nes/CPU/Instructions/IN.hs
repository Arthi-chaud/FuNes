module Nes.CPU.Instructions.IN (inx, iny) where

import Nes.CPU.Instructions.After
import Nes.CPU.Monad
import Nes.CPU.State

-- | Increment the value of the X register
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#INX
inx :: CPU r ()
inx = do
    newRegX <- (+ 1) <$> getRegister X
    setRegister X newRegX
    setZeroAndNegativeFlags newRegX

-- | Increment the value of the Y register
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#INY
iny :: CPU r ()
iny = do
    newRegY <- (+ 1) <$> getRegister Y
    setRegister Y newRegY
    setZeroAndNegativeFlags newRegY
