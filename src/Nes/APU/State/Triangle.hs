-- | Source: https://www.nesdev.org/wiki/APU_Triangle
module Nes.APU.State.Triangle (
    -- * Tiangle Definition
    Triangle (..),

    -- * Fields
    TriangleField,
    linearCounterControl,
    linearCounterLoad,
    timer,
    lengthCounterLoad,
) where

import Data.Bits
import Nes.APU.State.BitField
import Nes.APU.State.Channel
import Nes.APU.State.Common
import Nes.Memory

newtype Triangle = MkTriangle {unTriangle :: Channel} deriving (Eq, Show)

instance IsChannel Triangle where
    fromChannel = MkTriangle
    toChannel = unTriangle

type TriangleField a = BitField a Triangle

instance HasTimer Triangle
instance HasLengthCounterLoad Triangle

-- | Bit 7 of byte 1
linearCounterControl :: TriangleField Bool
linearCounterControl = singleBitField Byte1 7

-- | On bits 0-6 of byte 1
--
-- only the first 7 bits of the input will be used
linearCounterLoad :: TriangleField Byte
linearCounterLoad = MkBitField{..}
  where
    get = withChannelByte Byte1 (`clearBit` 7)
    set l = setChannelByte Byte1 (\b -> (b .&. 0b10000000) .|. (l `clearBit` 7))
