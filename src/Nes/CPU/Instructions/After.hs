-- | Common CPU Status update to do after instructions
module Nes.CPU.Instructions.After (setZeroAndNegativeFlags) where

import Data.Bits
import Data.Word
import Nes.CPU.Monad
import Nes.CPU.State

setZeroAndNegativeFlags :: Word8 -> CPU r ()
setZeroAndNegativeFlags res = do
    if res == 0 then setStatusFlag Zero else clearStatusFlag Zero
    if testBit res 7 then setStatusFlag Negative else clearStatusFlag Negative
