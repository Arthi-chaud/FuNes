{-# LANGUAGE RecordWildCards #-}

module Nes.APU.State.Pulse (
    -- * Pulse definition
    Pulse (..),
    PulseByte (..),
    setPulseByte,
    withPulseByte,

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
import Nes.APU.State.Internal
import Nes.Memory (Addr (unAddr), Byte (..), byteToAddr)

data Pulse = MkPulse
    { byte1 :: Byte
    -- ^ The first register byte of the pulse. E.g. if this is the first pulse register, Bus should be able to access it at 0x4000
    , byte2 :: Byte
    , byte3 :: Byte
    , byte4 :: Byte
    }
    deriving (Eq, Show)

-- | Enum to get a single byte from a 'Pulse'
data PulseByte
    = Byte1
    | Byte2
    | Byte3
    | Byte4
    deriving (Eq, Show)

-- | Update a given byte from a 'Pulse'
setPulseByte :: PulseByte -> (Byte -> Byte) -> Pulse -> Pulse
setPulseByte n f pulse = case n of
    Byte1 -> pulse{byte1 = f $ byte1 pulse}
    Byte2 -> pulse{byte2 = f $ byte2 pulse}
    Byte3 -> pulse{byte3 = f $ byte3 pulse}
    Byte4 -> pulse{byte4 = f $ byte4 pulse}

-- | Get a byte (and optionally do something with it) from a 'Pulse'
withPulseByte :: PulseByte -> (Byte -> a) -> Pulse -> a
withPulseByte n f pulse = case n of
    Byte1 -> f $ byte1 pulse
    Byte2 -> f $ byte2 pulse
    Byte3 -> f $ byte3 pulse
    Byte4 -> f $ byte4 pulse

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
        withPulseByte
            Byte1
            (`shiftR` 6)
    set byte =
        setPulseByte
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
    get = withPulseByte Byte1 first4bits
    set vol = setPulseByte Byte1 $ \b -> (b .&. 0b11110000) .|. first4bits vol
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
    get = withPulseByte Byte2 $ \b -> (b `shiftR` 4) .&. 0b111
    set vol = setPulseByte Byte2 $
        \b -> (b .&. 0b10001111) .|. ((vol .&. 0b111) `shiftL` 4)

-- | Sweep Shift. On bits 0-2 of byte 2 of Pulse
--
-- Note: Only the first 3 bits are taken
--
-- Source: https://www.nesdev.org/wiki/APU_Sweep
sweepShift :: PulseField Byte
sweepShift = MkBitField{..}
  where
    get = withPulseByte Byte2 $ \b -> b .&. 0b111
    set shiftCount = setPulseByte Byte2 $
        \b -> (b .&. 0b11111000) .|. (shiftCount .&. 0b111)

-- | Timer. On byte 3 of Pulse (low) and bits 0-2 of byte 4 (high)
--
-- Source: https://www.nesdev.org/wiki/APU#Pulse_($4000–$4007)
timer :: PulseField Word16
timer = MkBitField{..}
  where
    get pulse =
        let
            low = unAddr . byteToAddr $ withPulseByte Byte3 id pulse
            high = unAddr . byteToAddr $ withPulseByte Byte4 (.&. 0b111) pulse
         in
            (high `shiftL` 8) .|. low
    set w =
        let
            low = Byte . fromIntegral $ w .&. 0b11111111
            high = Byte . fromIntegral $ (w `shiftR` 8) .&. 0b111
         in
            setPulseByte Byte4 (\b -> (b .&. 0b11111000) .|. high)
                . setPulseByte Byte3 (const low)

-- | Length counter load. On bits 3-7 of byte 4 of Pulse
--
-- Source: https://www.nesdev.org/wiki/APU#Pulse_($4000–$4007)
lengthCounterLoad :: PulseField Byte
lengthCounterLoad = MkBitField{..}
  where
    get = withPulseByte Byte4 $ \b -> (b .&. 0b11111000) `shiftR` 3
    set l = setPulseByte Byte4 $ \b -> (l `shiftL` 3) .|. (b .&. 0b111)

-- | Util for single-bit fields
singleBitField :: PulseByte -> Int -> PulseField Bool
singleBitField byte off
    | off < 0 || off >= 8 = error "Invalid bit offset in byte"
    | otherwise = MkBitField{..}
  where
    get = withPulseByte byte $ toBool . getBit' off
    set b = setPulseByte byte $ setBit' (fromBool b) off
