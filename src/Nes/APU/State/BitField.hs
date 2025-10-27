module Nes.APU.State.BitField (
    BitField (..),
    Bit (..),
    fromBool,
    toBool,
    setBit',
    getBit',
) where

import Data.Bits

newtype Bit = MkBit {isSet :: Bool} deriving (Eq, Show, Bits)

fromBool :: Bool -> Bit
fromBool = MkBit

toBool :: Bit -> Bool
toBool = isSet

-- | Set bit of a value at given offset.
--
-- Avoids name clash
setBit' :: (Bits a) => Bit -> Int -> a -> a
setBit' (MkBit b) off a =
    if b
        then setBit a off
        else clearBit a off

-- | Get bit of a value at given offset.
--
-- Avoids name clash
getBit' :: (Bits a) => Int -> a -> Bit
getBit' off a = MkBit $ testBit a off

-- | bidirectional field getter/setter where the field is encoded in bits
data BitField fieldType obj = MkBitField
    { get :: obj -> fieldType
    , set :: fieldType -> obj -> obj
    }
