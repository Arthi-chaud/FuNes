module Nes.CPU.Instructions.Jump (jmp, jsr, rts, rti) where

import Control.Monad
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory

-- | Sets the program counter to the address specified by the operand
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP
jmp :: AddressingMode -> CPU r ()
jmp = getOperandAddr >=> setPC

-- | Jump to Subroutine
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#JSR
jsr :: CPU r ()
jsr = do
    pc <- getPC
    pushAddrStack (pc + 2 - 1)
    withBus (readAddr pc) >>= setPC

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
    setStatusFlag BreakCommand2
    setPC =<< popStackAddr

-- Note: Source for both: https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch3.3/src/cpu.rs#L703
