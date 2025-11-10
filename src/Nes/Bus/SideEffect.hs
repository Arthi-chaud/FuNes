module Nes.Bus.SideEffect (CPUSideEffect (..)) where

data CPUSideEffect = MkSE
    { setIRQ :: {-# UNPACK #-} !Bool
    , startDMCDMA :: {-# UNPACK #-} !Bool
    }
    deriving (Eq, Show)

instance Semigroup CPUSideEffect where
    (MkSE irq1 dma1) <> (MkSE irq2 dma2) = MkSE (irq1 || irq2) (dma1 || dma2)

instance Monoid CPUSideEffect where
    mempty = MkSE False False
