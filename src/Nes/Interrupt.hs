module Nes.Interrupt (Interrupt (..), getVectorAddr, getFlagMask, getCPUCycles) where

import Nes.Memory

data Interrupt = NMI | BRK deriving (Eq, Show)

getVectorAddr :: Interrupt -> Addr
getVectorAddr = \case
    NMI -> 0xfffa
    BRK -> 0xfffe

getFlagMask :: Interrupt -> Byte
getFlagMask = \case
    NMI -> 0b00100000
    BRK -> 0b00110000

getCPUCycles :: Interrupt -> Int
getCPUCycles = \case
    NMI -> 2
    BRK -> 1
