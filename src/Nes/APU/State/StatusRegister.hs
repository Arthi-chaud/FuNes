module Nes.APU.State.StatusRegister (StatusRegister (..), StatusRegisterFlag (..)) where

import Nes.FlagRegister
import Nes.Memory

newtype StatusRegister = MkSR {unSR :: Byte} deriving (Eq, Show)

data StatusRegisterFlag
    = Pulse1Enabled
    | Pulse2Enabled
    | TriangleEnabled
    | NoiseEnabled
    | DMCEnabled
    | Unused
    | FrameInterrupt
    | DMCInterrupt
    deriving (Eq, Show, Enum)

instance FlagRegister StatusRegister where
    type Flag StatusRegister = StatusRegisterFlag
    fromByte = MkSR
    toByte = unSR
    flagToBitOffset = fromEnum
