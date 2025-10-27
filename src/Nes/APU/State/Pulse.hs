{-# LANGUAGE RecordWildCards #-}

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
import Data.Word (Word16)
import Nes.APU.State.BitField
import Nes.APU.State.Channel
import Nes.Memory (Addr (unAddr), Byte (..), byteToAddr)

newtype Pulse = MkPulse {unPulse :: Channel} deriving (Eq, Show)

instance IsChannel Pulse where
    fromChannel = MkPulse
    toChannel = unPulse

type PulseField a = BitField a Pulse

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
        high = fromBool $ testBit byte 1
        low = fromBool $ testBit byte 0

-- | Envelope loop. Is on bit 5 of byte 1 of Pulse
--
-- Source: https://www.nesdev.org/wiki/APU#Pulse_($4000–$4007)
loops :: PulseField Bool
loops = singleBitField Byte1 5

-- | Volume is const . Is on bit 4 of byte 1 of Pulse
--
-- Source: https://www.nesdev.org/wiki/APU#Pulse_($4000–$4007)
volumeIsConst :: PulseField Bool
volumeIsConst = singleBitField Byte1 4

-- | Volume envelope. On first 4 bits of byte 1 of Pulse
--
-- Note: Only the 4 first bits of input are taken
--
-- Source: https://www.nesdev.org/wiki/APU#Pulse_($4000–$4007)
volume :: PulseField Byte
volume = MkBitField{..}
  where
    get = withChannelByte Byte1 first4bits
    set vol = setChannelByte Byte1 $ \b -> (b .&. 0b11110000) .|. first4bits vol
    first4bits b = b .&. 0b1111

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

-- | Timer. On byte 3 of Pulse (low) and bits 0-2 of byte 4 (high)
--
-- Source: https://www.nesdev.org/wiki/APU#Pulse_($4000–$4007)
timer :: PulseField Word16
timer = MkBitField{..}
  where
    get pulse =
        let
            low = unAddr . byteToAddr $ withChannelByte Byte3 id pulse
            high = unAddr . byteToAddr $ withChannelByte Byte4 (.&. 0b111) pulse
         in
            (high `shiftL` 8) .|. low
    set w =
        let
            low = Byte . fromIntegral $ w .&. 0b11111111
            high = Byte . fromIntegral $ (w `shiftR` 8) .&. 0b111
         in
            setChannelByte Byte4 (\b -> (b .&. 0b11111000) .|. high)
                . setChannelByte Byte3 (const low)

-- | Length counter load. On bits 3-7 of byte 4 of Pulse
--
-- Source: https://www.nesdev.org/wiki/APU#Pulse_($4000–$4007)
lengthCounterLoad :: PulseField Byte
lengthCounterLoad = MkBitField{..}
  where
    get = withChannelByte Byte4 $ \b -> (b .&. 0b11111000) `shiftR` 3
    set l = setChannelByte Byte4 $ \b -> (l `shiftL` 3) .|. (b .&. 0b111)

-- | Util for single-bit fields
singleBitField :: ChannelByte -> Int -> PulseField Bool
singleBitField byte off
    | off < 0 || off >= 8 = error "Invalid bit offset in byte"
    | otherwise = MkBitField{..}
  where
    get = withChannelByte byte $ toBool . getBit' off
    set b = setChannelByte byte $ setBit' (fromBool b) off
