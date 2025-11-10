module Nes.APU.BusInterface.Noise (write400C, write400E, write400F) where

import Data.Bits
import Nes.APU.Monad
import Nes.APU.State
import Nes.APU.State.Envelope
import Nes.APU.State.LengthCounter
import Nes.APU.State.Noise
import Nes.Memory

{-# INLINE write400C #-}
write400C :: Byte -> APU r ()
write400C byte = do
    let haltLC = byte `testBit` 5
        constVol = byte `testBit` 4
        vol = byte .&. 0b1111
    modifyAPUState $
        modifyNoise $
            withLengthCounter
                (\lc -> lc{isHalted = haltLC})
                . withEnvelope
                    (\e -> e{constantVolume = byteToInt vol, useConstantVolume = constVol, loopFlag = haltLC})

{-# INLINE write400E #-}
write400E :: Byte -> APU r ()
write400E byte = do
    let modeFlag = byte `testBit` 7
        periodIndex = byteToInt $ byte .&. 0b1111
    modifyAPUState $ modifyNoise $ \t -> t{period = getPeriodValue periodIndex, useBit6ForFeedback = modeFlag}

{-# INLINE write400F #-}
write400F :: Byte -> APU r ()
write400F byte = do
    let newLCLoad = byteToInt $ byte `shiftR` 3
    modifyAPUState $
        modifyNoise $
            withLengthCounter (loadLengthCounter newLCLoad)
                . withEnvelope (\e -> e{startFlag = True})
