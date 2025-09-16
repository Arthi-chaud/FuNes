module Nes.CPU.Instructions.ST (sta) where

import Nes.CPU.Instructions.Addressing
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory

sta :: AddressingMode -> CPU r ()
sta mode = do
    addr <- getOperandAddr mode
    regA <- withCPUState registerA
    withBus $ writeByte regA addr
