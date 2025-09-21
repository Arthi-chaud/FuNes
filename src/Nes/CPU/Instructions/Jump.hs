module Nes.CPU.Instructions.Jump (jmp, jsr, rts, rti) where

import Data.Bits
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory

-- | Sets the program counter to the address specified by the operand
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP
jmp :: AddressingMode -> CPU r ()
jmp Absolute = getPC >>= withBus . readAddr >>= setPC
-- See https://www.nesdev.org/wiki/Instruction_reference#JMP
-- And https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch3.4/src/cpu.rs#L692
jmp Indirect = do
    addr <- getPC >>= withBus . readAddr
    ref <-
        if addr .&. 0x00FF == 0x00FF
            then do
                low <- byteToAddr <$> withBus (readByte addr)
                high <- byteToAddr <$> withBus (readByte (addr .&. 0xff00))
                return $ shiftL high 8 .|. low
            else withBus $ readAddr addr
    setPC ref
jmp _ = fail "Unsupported addressing mode"

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
