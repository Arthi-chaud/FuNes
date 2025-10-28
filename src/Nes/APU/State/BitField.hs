module Nes.APU.State.BitField (
    BitField (..),
    setBit',
) where

import Data.Bits

-- | bidirectional field getter/setter where the field is encoded in bits
data BitField fieldType obj = MkBitField
    { get :: obj -> fieldType
    , set :: fieldType -> obj -> obj
    }

setBit' :: (Bits a) => Bool -> Int -> a -> a
setBit' bool = flip (if bool then setBit else clearBit)
