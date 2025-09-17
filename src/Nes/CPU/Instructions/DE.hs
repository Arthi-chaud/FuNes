module Nes.CPU.Instructions.DE (dex, dey) where

import Nes.CPU.Instructions.After (setZeroAndNegativeFlags)
import Nes.CPU.Monad
import Nes.CPU.State

-- | Decrement X register
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#DEX
dex :: CPU r ()
dex = do
    res <- (\x -> x - 1) <$> getRegister X
    setRegister X res
    setZeroAndNegativeFlags res

-- | Decrement Y register
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#DEY
dey :: CPU r ()
dey = do
    res <- (\y -> y - 1) <$> getRegister Y
    setRegister Y res
    setZeroAndNegativeFlags res
