module Nes.CPU.Instructions.SE (sec, sed, sei) where

import Nes.CPU.Monad
import Nes.CPU.State

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
