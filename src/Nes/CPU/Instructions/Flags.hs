module Nes.CPU.Instructions.Flags (clc, cld, cli, clv, sec, sed, sei) where

import Nes.CPU.Monad
import Nes.CPU.State
import Nes.FlagRegister
import Nes.Internal.MonadState

-- | Clear Carry Flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CLC
clc :: CPU r ()
clc = status %= clearFlag Carry

-- | Clear Decimal mode flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CLD
cld :: CPU r ()
cld = status %= clearFlag DecimalMode

-- | Clear Interrupt Disable
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CLI
cli :: CPU r ()
cli = status %= clearFlag InterruptDisable

-- | Clear Overflow Flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CLV
clv :: CPU r ()
clv = status %= clearFlag Overflow

-- | Set Carry Flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#SEC
sec :: CPU r ()
sec = status %= setFlag Carry

-- | Set Decimal mode flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#SED
sed :: CPU r ()
sed = status %= setFlag DecimalMode

-- | Set Interrupt Disable
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#SEI
sei :: CPU r ()
sei = status %= setFlag InterruptDisable
