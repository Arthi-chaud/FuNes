-- | Common CPU Status update to do after instructions
module Nes.CPU.Instructions.After (setZeroAndNegativeFlags) where

import Data.Bits
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.FlagRegister
import Nes.Memory

{-# INLINE setZeroAndNegativeFlags #-}
setZeroAndNegativeFlags :: Byte -> CPU r ()
setZeroAndNegativeFlags res =
    modifyCPUState $
        modifyStatusRegister $
            setFlag' Zero (res == 0)
                . setFlag' Negative (testBit res 7)
