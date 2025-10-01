module Nes.CPU.Instructions.Stack (pha, php, pla, plp) where

import Nes.CPU.Instructions.After
import Nes.CPU.Monad
import Nes.CPU.State

-- | Pushes a copy of the accumulator on to the stack.
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#PHA
pha :: CPU r ()
pha = tickOnce >> withCPUState (getRegister A) >>= pushByteStack

-- | Pushes a copy of the status flags on to the stack.
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#PHP
--
-- Source: https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch3.3/src/cpu.rs#L486
php :: CPU r ()
php = do
    st <-
        withCPUState
            ( status
                . setStatusFlag BreakCommand2
                . setStatusFlag BreakCommand
            )
    pushByteStack $ unSR st
    tickOnce

-- | Pulls an 8 bit value from the stack and into the accumulator.
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#PLA
pla :: CPU r ()
pla = do
    value <- popStackByte
    tick 2
    modifyCPUState $ setRegister A value
    setZeroAndNegativeFlags value

-- | Pulls an 8 bit value from the stack and into the accumulator.
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#PLP
--
-- Source: https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch3.3/src/cpu.rs#L478
plp :: CPU r ()
plp = do
    value <- popStackByte
    tick 2
    modifyCPUState $ \st -> st{status = MkSR value}
    modifyCPUState $
        clearStatusFlag BreakCommand
            . setStatusFlag BreakCommand2
