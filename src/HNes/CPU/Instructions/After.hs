-- | Common CPU Status update to do after instructions
module HNes.CPU.Instructions.After (setZeroAndNegativeFlags) where

import Data.Bits
import Data.Word
import HNes.CPU.Monad
import HNes.CPU.State

setZeroAndNegativeFlags :: Word8 -> CPU r ()
setZeroAndNegativeFlags res = do
    if res == 0 then setStatusFlag Zero else clearStatusFlag Zero
    if testBit res 7 then setStatusFlag Negative else clearStatusFlag Negative
