-- | Source: https://www.nesdev.org/wiki/APU#DMC_($4010–$4013)
module Nes.APU.State.DMC (
    DMC (..),
    DMCField,
    irqEnable,
    loopsDMC,
    frequency,
    loadCounter,
    sampleAddress,
    sampleLength,
) where

import Data.Bits
import Nes.APU.State.BitField
import Nes.APU.State.Channel
import Nes.APU.State.Common (singleBitField, singleByteField)
import Nes.Memory

newtype DMC = MkDMC {unDMC :: Channel} deriving (Eq, Show)

instance IsChannel DMC where
    toChannel = unDMC
    fromChannel = MkDMC

type DMCField a = BitField a DMC

-- | Bit 6 o byte 1. IRQ enabled flag. If clear, the interrupt flag is cleared.
irqEnable :: DMCField Bool
irqEnable = singleBitField Byte1 7

-- | Bit 5 of byte 1. Like 'loops' but specific to DMC
loopsDMC :: DMCField Bool
loopsDMC = singleBitField Byte1 6

-- | Bits 0-3 of byte 1
--
-- Only the first 4 bits of the value will be used
frequency :: DMCField Byte
frequency = MkBitField{..}
  where
    get = withChannelByte Byte1 (.&. 0b1111)
    set byte = setChannelByte Byte1 $ \b -> (b .&. 0b11110000) .|. (byte .&. 0b1111)

-- | Bits 0-5 of byte 2
--
-- Only the first 7 bits of the value will be used
loadCounter :: DMCField Byte
loadCounter = MkBitField{..}
  where
    get = withChannelByte Byte2 (`clearBit` 7)
    set byte = setChannelByte Byte2 $ \b -> (b .&. 0b10000000) .|. (byte `clearBit` 7)

-- | All bits of Byte 3
sampleAddress :: DMCField Byte
sampleAddress = singleByteField Byte3

-- | All bits of Byte 4
sampleLength :: DMCField Byte
sampleLength = singleByteField Byte4
