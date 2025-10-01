module Nes.CPU.Instructions.Access (
    -- * Load register value
    lda,
    ldx,
    ldy,

    -- * Store register value
    sta,
    stx,
    sty,
) where

import Control.Monad
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.After (setZeroAndNegativeFlags)
import Nes.CPU.Monad
import Nes.CPU.State hiding (getRegister, setRegister)
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
    param <- readByte argAddr ()
    setRegister register param
    setZeroAndNegativeFlags param

-- | Store the value of the A register in memory
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#STA
sta :: AddressingMode -> CPU r ()
sta = storeRegisterInMemory A

-- | Store the value of the X register in memory
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#STX
stx :: AddressingMode -> CPU r ()
stx = storeRegisterInMemory X

-- | Store the value of the Y register in memory
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#STY
sty :: AddressingMode -> CPU r ()
sty = storeRegisterInMemory Y

storeRegisterInMemory :: Register -> AddressingMode -> CPU r ()
storeRegisterInMemory reg mode = do
    (addr, crosses) <- getOperandAddr' mode
    value <- getRegister reg
    -- https://www.nesdev.org/wiki/Cycle_counting
    -- assumes the worst case of page crossing and always spends 1 extra read cycle.
    when (crosses || mode == AbsoluteX) tickOnce
    when (mode == IndirectY || mode == AbsoluteY) tickOnce
    writeByte value addr ()
