module Nes.CPU.Instructions.Stack (pha, php, pla, plp) where

import Nes.CPU.Instructions.After
import Nes.CPU.Monad
import Nes.CPU.State

-- | Pushes a copy of the accumulator on to the stack.
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#PHA
pha :: CPU r ()
pha = getRegister A >>= pushByteStack

-- | Pushes a copy of the status flags on to the stack.
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#PHP
php :: CPU r ()
php = withCPUState status >>= pushByteStack

-- | Pulls an 8 bit value from the stack and into the accumulator.
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#PLA
pla :: CPU r ()
pla = do
    value <- popStackByte
    setRegister A value
    setZeroAndNegativeFlags value

-- | Pulls an 8 bit value from the stack and into the accumulator.
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#PLP
plp :: CPU r ()
plp = do
    value <- popStackByte
    modifyCPUState $ \st -> st{status = value}
