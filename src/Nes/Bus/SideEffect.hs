module Nes.Bus.SideEffect (CPUSideEffect (..), CPUSideEffectFlag (..)) where

import Data.Bits ((.|.))
import Nes.FlagRegister
import Nes.Memory

newtype CPUSideEffect = MkSE {unSE :: Byte}

data CPUSideEffectFlag = IRQ | DMCDMA deriving (Eq, Show, Enum)

instance FlagRegister CPUSideEffect where
    type Flag CPUSideEffect = CPUSideEffectFlag
    fromByte = MkSE
    toByte = unSE
    flagToBitOffset = fromEnum

instance Semigroup CPUSideEffect where
    {-# INLINE (<>) #-}
    MkSE se1 <> MkSE se2 = MkSE (se1 .|. se2)

instance Monoid CPUSideEffect where
    {-# INLINE mempty #-}
    mempty = MkSE 0
