{-# LANGUAGE RecordWildCards #-}

module Nes.APU.State.Pulse where

import Data.Bits (Bits (..))
import Data.Word (Word16)
import Nes.APU.State.Internal
import Nes.Memory (Addr (unAddr), Byte (..), byteToAddr)

data Pulse = MkPulse
    { byte1 :: Byte
    , byte2 :: Byte
    , byte3 :: Byte
    , byte4 :: Byte
    }

data PulseByte
    = Byte1
    | Byte2
    | Byte3
    | Byte4
    deriving (Eq, Show)

setPulseByte :: PulseByte -> (Byte -> Byte) -> Pulse -> Pulse
setPulseByte n f pulse = case n of
    Byte1 -> pulse{byte1 = f $ byte1 pulse}
    Byte2 -> pulse{byte2 = f $ byte2 pulse}
    Byte3 -> pulse{byte3 = f $ byte3 pulse}
    Byte4 -> pulse{byte4 = f $ byte4 pulse}

withPulseByte :: PulseByte -> (Byte -> a) -> Pulse -> a
withPulseByte n f pulse = case n of
    Byte1 -> f $ byte1 pulse
    Byte2 -> f $ byte2 pulse
    Byte3 -> f $ byte3 pulse
    Byte4 -> f $ byte4 pulse

duty :: BitField (Bit, Bit) Pulse
duty = MkBitField{..}
  where
    get =
        withPulseByte
            Byte1
            (\b -> (getBit' 7 b, getBit' 6 b))
    set (high, low) =
        setPulseByte
            Byte1
            (setBit' high 7 . setBit' low 6)

loops :: BitField Bool Pulse
loops = singleBitField Byte1 5

volumeIsConst :: BitField Bool Pulse
volumeIsConst = singleBitField Byte1 4

-- | Volume envelope.
--
-- Only the 4 first bits are taken
volume :: BitField Byte Pulse
volume = MkBitField{..}
  where
    get = withPulseByte Byte1 first4bits
    set vol = setPulseByte Byte1 $ \b -> (b .&. 0b00001111) .|. first4bits vol
    first4bits b = b .&. 0b1111

sweepEnabled :: BitField Bool Pulse
sweepEnabled = singleBitField Byte2 7

sweepNegate :: BitField Bool Pulse
sweepNegate = singleBitField Byte2 3

-- | Sweep Period
--
-- The divider's period is P + 1 half-frames
--
-- Only the first 3 bits are taken
sweepPeriod :: BitField Byte Pulse
sweepPeriod = MkBitField{..}
  where
    get = withPulseByte Byte2 $ \b -> (b `shiftR` 4) .&. 0b111
    set vol = setPulseByte Byte1 $
        \b -> (b .&. 0b10001111) .|. ((vol .&. 0b111) `shiftL` 4)

-- | Sweep Shift
--
-- Only the first 3 bits are taken
sweepShift :: BitField Byte Pulse
sweepShift = MkBitField{..}
  where
    get = withPulseByte Byte2 $ \b -> b .&. 0b111
    set shiftCount = setPulseByte Byte1 $
        \b -> (b .&. 0b11111000) .|. (shiftCount .&. 0b111)

singleBitField :: PulseByte -> Int -> BitField Bool Pulse
singleBitField byte off
    | off < 0 || off >= 8 = error "Invalid bit offset in byte"
    | otherwise = MkBitField{..}
  where
    get = withPulseByte byte $ toBool . getBit' off
    set b = setPulseByte byte $ setBit' (fromBool b) off

timer :: BitField Word16 Pulse
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
            setPulseByte Byte4 (\b -> (b .&. 0b11111000) .&. high)
                . setPulseByte Byte3 (const low)

lengthCounterLoad :: BitField Byte Pulse
lengthCounterLoad = MkBitField{..}
  where
    get = withPulseByte Byte4 $ \b -> (b .&. 0b11111000) `shiftR` 3
    set l = setPulseByte Byte4 $ \b -> (b .&. 0b111) .|. (l `shiftL` 3)
