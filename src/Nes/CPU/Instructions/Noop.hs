module Nes.CPU.Instructions.Noop (noop) where

import Control.Monad
import Nes.CPU.Instructions.Addressing (AddressingMode (..), getOperandAddr)
import Nes.CPU.Monad

noop :: AddressingMode -> CPU r ()
noop = \case
    None -> pure ()
    Accumulator -> pure ()
    mode -> void $ getOperandAddr mode
