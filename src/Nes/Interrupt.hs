module Nes.Interrupt (Interrupt (..), getVectorAddr, getFlagMask, getCPUCycles) where

import Nes.Memory

data Interrupt = NMI deriving (Eq, Show) -- TODO Add BRK

getVectorAddr :: Interrupt -> Addr
getVectorAddr = \case
    NMI -> 0xfffa

getFlagMask :: Interrupt -> Byte
getFlagMask = \case
    NMI -> 0b00100000

getCPUCycles :: Interrupt -> Int
getCPUCycles = \case
    NMI -> 2
