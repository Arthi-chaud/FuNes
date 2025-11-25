module Nes.CPU.Instructions.Jump (
    -- * Jump
    jmp,
    jsr,

    -- * Return
    rts,
    rti,
) where

import Data.Bits
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Internal.MonadState
import Nes.Memory

-- | Sets the program counter to the address specified by the operand
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP
jmp :: AddressingMode -> CPU r ()
jmp Absolute = usesM pc (`readAddr` ()) >>= (pc .=)
-- See https://www.nesdev.org/wiki/Instruction_reference#JMP
-- And https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch3.4/src/cpu.rs#L692
jmp Indirect = do
    addr <- usesM pc (`readAddr` ())
    ref <-
        if addr .&. 0x00FF == 0x00FF
            then do
                low <- byteToAddr <$> readByte addr ()
                high <- byteToAddr <$> readByte (addr .&. 0xff00) ()
                return $ shiftL high 8 .|. low
            else readAddr addr ()
    pc .= ref
jmp _ = fail "Unsupported addressing mode"

-- | Jump to Subroutine
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#JSR
jsr :: CPU r ()
jsr = do
    pc' <- use pc
    pushAddrStack (pc' + 2 - 1)
    tickOnce
    (pc .=) =<< readAddr pc' ()

-- | Return from Subroutine
--
-- Pulls the PC from the stack
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#RTS
rts :: CPU r ()
rts = do
    tick 2
    res <- popStackAddr
    -- https://www.nesdev.org/wiki/Cycle_counting
    --  plus 1 cycle to post-increment the program counter
    tickOnce
    pc .= (res + 1)

-- | Return from interrupt
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#RTI
rti :: CPU r ()
rti = do
    popStatusRegister
    (pc .=) =<< popStackAddr
    tick 2
