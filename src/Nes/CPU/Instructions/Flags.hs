module Nes.CPU.Instructions.Flags (clc, cld, cli, clv, sec, sed, sei) where

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

-- | Set Carry Flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#SEC
sec :: CPU r ()
sec = setStatusFlag Carry

-- | Set Decimal mode flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#SED
sed :: CPU r ()
sed = setStatusFlag DecimalMode

-- | Set Interrupt Disable
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#SEI
sei :: CPU r ()
sei = setStatusFlag InteruptDisable
