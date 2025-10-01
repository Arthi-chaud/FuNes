-- | Common CPU Status update to do after instructions
module Nes.CPU.Instructions.After (setZeroAndNegativeFlags) where

import Data.Bits
import Nes.CPU.Monad
import Nes.CPU.State (StatusRegisterFlag (..))
import Nes.Memory

setZeroAndNegativeFlags :: Byte -> CPU r ()
setZeroAndNegativeFlags res = do
    setStatusFlag' Zero $ res == 0
    setStatusFlag' Negative $ testBit res 7
