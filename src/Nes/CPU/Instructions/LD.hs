module Nes.CPU.Instructions.LD (lda, ldx, ldy) where

import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.After (setZeroAndNegativeFlags)
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory

-- | Load Register A
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#LDA
lda :: AddressingMode -> CPU r ()
lda = loadRegisterFromMemory A

-- | Load Register X
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#LDX
ldx :: AddressingMode -> CPU r ()
ldx = loadRegisterFromMemory X

-- | Load Register Y
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#LDY
ldy :: AddressingMode -> CPU r ()
ldy = loadRegisterFromMemory Y

loadRegisterFromMemory :: Register -> AddressingMode -> CPU r ()
loadRegisterFromMemory register mode = do
    argAddr <- getOperandAddr mode
    param <- withBus $ readByte argAddr
    setRegister register param
    setZeroAndNegativeFlags param
