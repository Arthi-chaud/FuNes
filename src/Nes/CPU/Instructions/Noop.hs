module Nes.CPU.Instructions.Noop (noop) where

import Control.Monad
import Nes.CPU.Instructions.Addressing (AddressingMode (..), getOperandAddr)
import Nes.CPU.Monad
import Nes.Memory (MemoryInterface (readByte))

noop :: AddressingMode -> CPU r ()
noop = \case
    None -> pure ()
    Accumulator -> pure ()
    mode -> do
        addr <- getOperandAddr mode
        void $ readByte addr ()
