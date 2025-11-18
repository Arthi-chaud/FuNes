module Nes.CPU.Instructions.Interrupt (brk) where

import Nes.CPU.Monad
import Nes.Interrupt

brk :: CPU r ()
brk = do
    incrementPC
    modifyInterruptStatus $ \s -> s{irq = Just BRK}
