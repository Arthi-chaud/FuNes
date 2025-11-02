module Nes.APU.State.FrameCounter (
    -- * Definition
    FrameCounter (..),

    -- * Fields
    mode,
    inhibitIRQ,
) where

import Data.Bits (Bits (testBit))
import Nes.APU.State.BitField
import Nes.Memory

newtype FrameCounter = MkFC {unFC :: Byte} deriving (Eq, Show)

data Mode = FourStep | FiveStep deriving (Eq, Show, Enum)

type FrameCounterField a = BitField a FrameCounter

mode :: FrameCounterField Mode
mode = MkBitField{..}
  where
    set m = MkFC . setBit' (trEnum m) 7 . unFC
    get = trEnum . (`testBit` 7) . unFC
    trEnum :: (Enum a, Enum b) => a -> b
    trEnum = toEnum . fromEnum

inhibitIRQ :: FrameCounterField Bool
inhibitIRQ = MkBitField{..}
  where
    get = (`testBit` 6) . unFC
    set bool = MkFC . setBit' bool 6 . unFC
