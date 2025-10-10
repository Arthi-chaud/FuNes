module Nes.CPU.Instructions.Noop (noop, kil) where

import Control.Monad
import Nes.CPU.Instructions.Addressing (AddressingMode (..), getOperandAddr)
import Nes.CPU.Monad
import Nes.Memory (MemoryInterface (readByte))

-- | No-op
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#NOP
noop :: AddressingMode -> CPU r ()
noop = \case
    None -> pure ()
    Accumulator -> pure ()
    mode -> do
        addr <- getOperandAddr mode
        void $ readByte addr ()

-- | AKA Jam
kil :: CPU r ()
kil = pure ()
