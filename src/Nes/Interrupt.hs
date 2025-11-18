module Nes.Interrupt (
    -- * Interrupt Enum
    InterruptStatus (..),
    IRQSource (..),
) where

data IRQSource = BRK | DMC | FrameCounter deriving (Eq, Show)

data InterruptStatus = MkIS
    { irq :: {-# UNPACK #-} !(Maybe IRQSource)
    , nmi :: {-# UNPACK #-} !Bool
    }
