module Nes.APU.BusInterface.DMC (write4010, write4011, write4012, write4013) where

import Data.Bits
import Nes.APU.Monad
import Nes.APU.State (modifyDMC)
import Nes.APU.State.DMC
import Nes.Memory

{-# INLINE write4010 #-}
write4010 :: Byte -> APU r ()
write4010 byte = do
    let irq = byte `testBit` 7
        loop = byte `testBit` 6
        rateIdx = byteToInt $ byte .&. 0b1111
        rate = getPeriodValue rateIdx
    modifyAPUState $ modifyDMC $ \dmc ->
        dmc
            { irqEnabledFlag = irq
            , loopFlag = loop
            , period = rate
            }

{-# INLINE write4011 #-}
write4011 :: Byte -> APU r ()
write4011 byte = do
    let directLoad = byteToInt $ byte .&. 0b1111111
    -- TODO If the timer is outputting a clock at the same time, the output level is occasionally not changed properly.
    modifyAPUState $ modifyDMC $ \dmc -> dmc{outputLevel = directLoad}

{-# INLINE write4012 #-}
write4012 :: Byte -> APU r ()
write4012 byte = do
    let sampleAddr = 0xC000 + (byteToAddr byte * 64)
    modifyAPUState $ modifyDMC $ \dmc -> dmc{sampleOgAddr = sampleAddr}

{-# INLINE write4013 #-}
write4013 :: Byte -> APU r ()
write4013 byte = do
    let sampleLength = (byteToInt byte * 16) + 1
    modifyAPUState $ modifyDMC $ \dmc -> dmc{sampleOgLength = sampleLength}
