module Nes.CPU.Instructions.CL (clc, cld, cli, clv) where

import Nes.CPU.Monad
import Nes.CPU.State

-- | Clear Carry Flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CLC
clc :: CPU r ()
clc = clearStatusFlag Carry

-- | Clear Decimal mode flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CLD
cld :: CPU r ()
cld = clearStatusFlag DecimalMode

-- | Clear Interrupt Disable
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CLI
cli :: CPU r ()
cli = clearStatusFlag InteruptDisable

-- | Clear Overflow Flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CLV
clv :: CPU r ()
clv = clearStatusFlag Overflow
