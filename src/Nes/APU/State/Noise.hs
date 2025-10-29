module Nes.APU.State.Noise (Noise (..), noiseMode, noisePeriod) where

import Data.Bits
import Nes.APU.State.BitField
import Nes.APU.State.Channel
import Nes.APU.State.Common
import Nes.Memory

newtype Noise = MkNoise {unNoise :: Channel} deriving (Eq, Show)

instance IsChannel Noise where
    fromChannel = MkNoise
    toChannel = unNoise

type NoiseField a = BitField a Noise

instance HasLengthCounterLoad Noise
instance HasVolume Noise
instance HasLoop Noise

-- | Bit 7 of byte 3
--
-- Source: https://www.nesdev.org/wiki/APU#Noise_($400C–$400F)
noiseMode :: NoiseField Bool
noiseMode = singleBitField Byte3 7

-- | Bits 0-3 of byte 3
--
--  Only first 4 bits of input are used
--
-- Source: https://www.nesdev.org/wiki/APU#Noise_($400C–$400F)
noisePeriod :: NoiseField Byte
noisePeriod = MkBitField{..}
  where
    get = withChannelByte Byte3 $ \b -> b .&. 0b1111
    set byte = setChannelByte Byte3 $ \b -> (b .&. 0b11110000) .|. (byte .&. 0b1111)
