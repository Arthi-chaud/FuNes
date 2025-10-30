module Nes.APU.State.Common (
    -- * Timer
    HasTimer,
    timer,

    -- * LCL
    HasLengthCounterLoad,
    lengthCounterLoad,

    -- * Noise
    HasVolume,
    volume,
    volumeIsConst,

    -- * Loop
    HasLoop,
    loops,

    -- * Util
    singleBitField,
    singleByteField,
) where

import Data.Bits
import Data.Word
import Nes.APU.State.BitField
import Nes.APU.State.Channel
import Nes.Memory

class (IsChannel a) => HasTimer a

-- | Timer. On byte 3 of channel (low) and bits 0-2 of byte 4 (high)
--
-- Source: https://www.nesdev.org/wiki/APU#Pulse_($4000–$4007)
timer :: (IsChannel a) => BitField Word16 a
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

class (IsChannel a) => HasLengthCounterLoad a

-- | Length counter load. On bits 3-7 of byte 4 of channel
--
-- Source: https://www.nesdev.org/wiki/APU#Pulse_($4000–$4007)
lengthCounterLoad :: (IsChannel a) => BitField Byte a
lengthCounterLoad = MkBitField{..}
  where
    get = withChannelByte Byte4 $ \b -> (b .&. 0b11111000) `shiftR` 3
    set l = setChannelByte Byte4 $ \b -> (l `shiftL` 3) .|. (b .&. 0b111)

class (IsChannel a) => HasVolume a

-- | Volume envelope. On first 4 bits of byte 1 of channel
--
-- Note: Only the 4 first bits of input are taken
--
-- Source: https://www.nesdev.org/wiki/APU#Pulse_($4000–$4007)
volume :: (HasVolume a) => BitField Byte a
volume = MkBitField{..}
  where
    get = withChannelByte Byte1 first4bits
    set vol = setChannelByte Byte1 $ \b -> (b .&. 0b11110000) .|. first4bits vol
    first4bits b = b .&. 0b1111

-- | Volume is const . Is on bit 4 of byte 1 of channel
--
-- Source: https://www.nesdev.org/wiki/APU#Pulse_($4000–$4007)
volumeIsConst :: (HasVolume a) => BitField Bool a
volumeIsConst = singleBitField Byte1 4

class (IsChannel a) => HasLoop a

-- | Envelope loop. Is on bit 5 of byte 1 of channel
--
-- Source: https://www.nesdev.org/wiki/APU#Pulse_($4000–$4007)
loops :: (HasLoop a) => BitField Bool a
loops = singleBitField Byte1 5

-- | Util for single-bit fields
singleBitField :: (IsChannel a) => ChannelByte -> Int -> BitField Bool a
singleBitField byte off
    | off < 0 || off >= 8 = error "Invalid bit offset in byte"
    | otherwise = MkBitField{..}
  where
    get = withChannelByte byte (`testBit` off)
    set b = setChannelByte byte $ setBit' b off

-- | Util for fields that take exactly one byte
singleByteField :: (IsChannel a) => ChannelByte -> BitField Byte a
singleByteField byte = MkBitField{..}
  where
    get = withChannelByte byte id
    set b = setChannelByte byte $ const b
