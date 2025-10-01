module Nes.CPU.Instructions.Branch (bvc, bvs, bcc, bcs, beq, bne, bmi, bpl) where

import Control.Monad
import Nes.CPU.Instructions.Addressing (AddressingMode, getOperandAddr')
import Nes.CPU.Monad hiding (getStatusFlag)
import Nes.CPU.State

-- | Branch over if Overflow flag is clear
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BVC
bvc :: AddressingMode -> CPU r ()
bvc = branchOverIf (not . getStatusFlag Overflow)

-- | Branch over if Overflow flag is set
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BVS
bvs :: AddressingMode -> CPU r ()
bvs = branchOverIf (getStatusFlag Overflow)

-- | Branch over if Carry is clear
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BCC
bcc :: AddressingMode -> CPU r ()
bcc = branchOverIf (not . getStatusFlag Carry)

-- | Branch over if Carry is clear
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BCS
bcs :: AddressingMode -> CPU r ()
bcs = branchOverIf (getStatusFlag Carry)

-- | Branch over if zero flag is set
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BEQ
beq :: AddressingMode -> CPU r ()
beq = branchOverIf (getStatusFlag Zero)

-- | Branch over if zero flag is no set
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BNE
bne :: AddressingMode -> CPU r ()
bne = branchOverIf (not . getStatusFlag Zero)

-- | Branch over if negative flag is set
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BMI
bmi :: AddressingMode -> CPU r ()
bmi = branchOverIf (getStatusFlag Negative)

-- | Branch over if negative flag is not set
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BPL
bpl :: AddressingMode -> CPU r ()
bpl = branchOverIf (not . getStatusFlag Negative)

branchOverIf :: (CPUState -> Bool) -> AddressingMode -> CPU r ()
branchOverIf check mode = do
    doBranch <- withCPUState check
    (addr, crosses) <- getOperandAddr' mode
    when doBranch $ do
        when crosses tickOnce -- https://www.nesdev.org/obelisk-6502-guide/reference.html#BEQ
        tickOnce >> setPC addr
