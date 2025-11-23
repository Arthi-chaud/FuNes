module Nes.CPU.Instructions.Interrupt (brk) where

import Nes.CPU.Monad
import Nes.Internal.MonadState
import Nes.Interrupt

brk :: CPU r ()
brk = do
    incrementPC
    modify $ \s -> s{irq = Just BRK}
