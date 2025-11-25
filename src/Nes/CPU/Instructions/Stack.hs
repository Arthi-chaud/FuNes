module Nes.CPU.Instructions.Stack (pha, php, pla, plp) where

import Nes.CPU.Instructions.After
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Internal.MonadState

-- | Pushes a copy of the accumulator on to the stack.
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#PHA
pha :: CPU r ()
pha = tickOnce >> use registerA >>= pushByteStack

-- | Pushes a copy of the status flags on to the stack.
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#PHP
--
-- Source: https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch3.3/src/cpu.rs#L486
php :: CPU r ()
php = pushStatusRegister True >> tickOnce

-- | Pulls an 8 bit value from the stack and into the accumulator.
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#PLA
pla :: CPU r ()
pla = do
    value <- popStackByte
    tick 2
    registerA .= value
    setZeroAndNegativeFlags value

-- | Pulls an 8 bit value from the stack and into the accumulator.
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#PLP
plp :: CPU r ()
plp = popStatusRegister >> tick 2
