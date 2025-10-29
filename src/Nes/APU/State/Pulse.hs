module Nes.APU.State.Pulse (
    -- * Pulse definition
    Pulse (..),

    -- * Fields
    PulseField,
    duty,
    loops,
    volumeIsConst,
    volume,
    sweepEnabled,
    sweepPeriod,
    sweepNegate,
    sweepShift,
    timer,
    lengthCounterLoad,
) where

import Data.Bits (Bits (..))
import Nes.APU.State.BitField
import Nes.APU.State.Channel
import Nes.APU.State.Common
import Nes.Memory (Byte (..))

newtype Pulse = MkPulse {unPulse :: Channel} deriving (Eq, Show)

instance IsChannel Pulse where
    fromChannel = MkPulse
    toChannel = unPulse

type PulseField a = BitField a Pulse

instance HasTimer Pulse
instance HasLengthCounterLoad Pulse
instance HasVolume Pulse
instance HasLoop Pulse

-- | Duty cycles. Is on bits 6 and 7 of byte 1 of Pulse
--
-- Note: Only the first 2 bits of the input Byte are considered
--
-- Source: https://www.nesdev.org/wiki/APU#Pulse_($4000–$4007)
duty :: PulseField Byte
duty = MkBitField{..}
  where
    get =
        withChannelByte
            Byte1
            (`shiftR` 6)
    set byte =
        setChannelByte
            Byte1
            (setBit' high 7 . setBit' low 6)
      where
        high = testBit byte 1
        low = testBit byte 0

-- | Sweep is enabled. On bit 7 of byte 2 of Pulse
--
-- Source: https://www.nesdev.org/wiki/APU_Sweep
sweepEnabled :: PulseField Bool
sweepEnabled = singleBitField Byte2 7

-- | Sweep is negated. On bit 3 of byte 2 of Pulse
--
-- Source: https://www.nesdev.org/wiki/APU_Sweep
sweepNegate :: PulseField Bool
sweepNegate = singleBitField Byte2 3

-- | Sweep Period. On bits 4-6 of byte 2 of Pulse
--
-- The divider's period is P + 1 half-frames
--
-- Note: Only the first 3 bits are taken
--
-- Source: https://www.nesdev.org/wiki/APU_Sweep
sweepPeriod :: PulseField Byte
sweepPeriod = MkBitField{..}
  where
    get = withChannelByte Byte2 $ \b -> (b `shiftR` 4) .&. 0b111
    set vol = setChannelByte Byte2 $
        \b -> (b .&. 0b10001111) .|. ((vol .&. 0b111) `shiftL` 4)

-- | Sweep Shift. On bits 0-2 of byte 2 of Pulse
--
-- Note: Only the first 3 bits are taken
--
-- Source: https://www.nesdev.org/wiki/APU_Sweep
sweepShift :: PulseField Byte
sweepShift = MkBitField{..}
  where
    get = withChannelByte Byte2 $ \b -> b .&. 0b111
    set shiftCount = setChannelByte Byte2 $
        \b -> (b .&. 0b11111000) .|. (shiftCount .&. 0b111)
