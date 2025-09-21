module Nes.CPU.Instructions.Branch (bvc, bvs, bcc, bcs, beq, bne, bmi, bpl) where

import Control.Monad
import Nes.CPU.Instructions.Addressing (AddressingMode, getOperandAddr)
import Nes.CPU.Monad
import Nes.CPU.State

-- | Branch over if Overflow flag is clear
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BVC
bvc :: AddressingMode -> CPU r ()
bvc = branchOverIf (not . getStatusFlagPure Overflow)

-- | Branch over if Overflow flag is set
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BVS
bvs :: AddressingMode -> CPU r ()
bvs = branchOverIf (getStatusFlagPure Overflow)

-- | Branch over if Carry is clear
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BCC
bcc :: AddressingMode -> CPU r ()
bcc = branchOverIf (not . getStatusFlagPure Carry)

-- | Branch over if Carry is clear
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BCS
bcs :: AddressingMode -> CPU r ()
bcs = branchOverIf (getStatusFlagPure Carry)

-- | Branch over if zero flag is set
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BEQ
beq :: AddressingMode -> CPU r ()
beq = branchOverIf (getStatusFlagPure Zero)

-- | Branch over if zero flag is no set
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BNE
bne :: AddressingMode -> CPU r ()
bne = branchOverIf (not . getStatusFlagPure Zero)

-- | Branch over if negative flag is set
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BMI
bmi :: AddressingMode -> CPU r ()
bmi = branchOverIf (getStatusFlagPure Negative)

-- | Branch over if negative flag is not set
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BPL
bpl :: AddressingMode -> CPU r ()
bpl = branchOverIf (not . getStatusFlagPure Negative)

branchOverIf :: (CPUState -> Bool) -> AddressingMode -> CPU r ()
branchOverIf check mode = do
    doBranch <- withCPUState check
    addr <- getOperandAddr mode
    when doBranch $ setPC addr
