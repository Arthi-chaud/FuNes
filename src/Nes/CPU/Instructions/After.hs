-- | Common CPU Status update to do after instructions
module Nes.CPU.Instructions.After (setZeroAndNegativeFlags) where

import Data.Bits
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory

setZeroAndNegativeFlags :: Byte -> CPU r ()
setZeroAndNegativeFlags res = do
    if res == 0 then setStatusFlag Zero else clearStatusFlag Zero
    if testBit res 7 then setStatusFlag Negative else clearStatusFlag Negative
