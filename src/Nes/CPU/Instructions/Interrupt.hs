module Nes.CPU.Instructions.Interrupt (brk) where

import Control.Monad
import Nes.CPU.Monad
import Nes.CPU.State (CPUState (status), StatusRegisterFlag (InterruptDisable))
import Nes.FlagRegister (getFlag)
import Nes.Interrupt

brk :: CPU r ()
brk = do
    incrementPC
    interruptDisable <- withCPUState $ getFlag InterruptDisable . status
    unless interruptDisable $ interrupt BRK
