module Nes.CPU.Instructions.BV (bvc, bvs) where

import Nes.CPU.Instructions.Addressing
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

branchOverIf :: (CPUState -> Bool) -> AddressingMode -> CPU r ()
branchOverIf check mode = do
    doBranch <- withCPUState check
    if doBranch
        then getOperandAddr mode >>= setPC
        else incrementPC
