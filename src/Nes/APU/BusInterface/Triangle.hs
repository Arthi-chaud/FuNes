module Nes.APU.BusInterface.Triangle (
    -- * Triangle
    write4008,
    write400A,
    write400B,
) where

import Data.Bits
import Nes.APU.Monad
import Nes.APU.State
import Nes.APU.State.LengthCounter
import Nes.APU.State.Triangle
import Nes.Internal.MonadState
import Nes.Memory

{-# INLINE write4008 #-}
write4008 :: Byte -> APU r ()
write4008 byte = do
    let control = byte `testBit` 7
        reload = byteToInt $ byte `clearBit` 7
    modify $
        modifyTriangle $
            withLengthCounter (\lc -> lc{isHalted = control})
                . \t -> t{controlFlag = control, reloadValue = reload}

{-# INLINE write400A #-}
write400A :: Byte -> APU r ()
write400A periodLow = modify $ modifyTriangle $ \t ->
    let newPeriod = (period t .&. 0b11100000000) .|. byteToInt periodLow
     in t{period = newPeriod}

{-# INLINE write400B #-}
write400B :: Byte -> APU r ()
write400B byte = modify $ modifyTriangle $ \t ->
    let timerHigh = byteToInt $ byte .&. 0b111
        newPeriod = (timerHigh `shiftL` 8) .|. (period t .&. 0b11111111)
        newLcLoad = byteToInt $ byte `shiftR` 3
     in withLengthCounter (loadLengthCounter newLcLoad) $
            t
                { reloadFlag = True
                , period = newPeriod
                }
