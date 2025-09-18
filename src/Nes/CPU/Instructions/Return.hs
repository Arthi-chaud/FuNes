module Nes.CPU.Instructions.Return (rts, rti) where

import Nes.CPU.Monad (CPU, clearStatusFlag, modifyCPUState, popStackAddr, popStackByte, setPC)
import Nes.CPU.State

-- | Return from Subroutine
--
-- Pulls the PC from the stack
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#RTS
rts :: CPU r ()
rts = setPC . (+ 1) =<< popStackAddr

-- | Return from interrupt
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#RTI
rti :: CPU r ()
rti = do
    newStatus <- popStackByte
    modifyCPUState (\st -> st{status = newStatus})
    clearStatusFlag BreakCommand
    setPC =<< popStackAddr

-- Note: Source for both: https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch3.3/src/cpu.rs#L703
