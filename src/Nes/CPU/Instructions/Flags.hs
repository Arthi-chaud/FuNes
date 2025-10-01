module Nes.CPU.Instructions.Flags (clc, cld, cli, clv, sec, sed, sei) where

import Nes.CPU.Monad
import Nes.CPU.State
import Nes.FlagRegister

-- | Clear Carry Flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CLC
clc :: CPU r ()
clc = modifyCPUState $ modifyStatusRegister $ clearFlag Carry

-- | Clear Decimal mode flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CLD
cld :: CPU r ()
cld = modifyCPUState $ modifyStatusRegister $ clearFlag DecimalMode

-- | Clear Interrupt Disable
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CLI
cli :: CPU r ()
cli = modifyCPUState $ modifyStatusRegister $ clearFlag InteruptDisable

-- | Clear Overflow Flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#CLV
clv :: CPU r ()
clv = modifyCPUState $ modifyStatusRegister $ clearFlag Overflow

-- | Set Carry Flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#SEC
sec :: CPU r ()
sec = modifyCPUState $ modifyStatusRegister $ setFlag Carry

-- | Set Decimal mode flag
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#SED
sed :: CPU r ()
sed = modifyCPUState $ modifyStatusRegister $ setFlag DecimalMode

-- | Set Interrupt Disable
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#SEI
sei :: CPU r ()
sei = modifyCPUState $ modifyStatusRegister $ setFlag InteruptDisable
