module Nes.APU.State.Common (
    -- * Timer
    HasTimer,
    timer,

    -- * LCL
    HasLengthCounterLoad,
    lengthCounterLoad,

    -- * Util
    singleBitField,
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

-- | Util for single-bit fields
singleBitField :: (IsChannel a) => ChannelByte -> Int -> BitField Bool a
singleBitField byte off
    | off < 0 || off >= 8 = error "Invalid bit offset in byte"
    | otherwise = MkBitField{..}
  where
    get = withChannelByte byte (`testBit` off)
    set b = setChannelByte byte $ setBit' b off
