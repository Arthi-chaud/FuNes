module Nes.CPU.Interrupt (handleInterrupt) where

import Data.Bits
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.FlagRegister
import Nes.Interrupt
import Nes.Memory

handleInterrupt :: CPU r ()
handleInterrupt = do
    maskInterrupt <- withCPUState $ getFlag InterruptDisable . status
    pendingInterrupt <- popInterrupt
    case pendingInterrupt of
        Nothing -> pure ()
        Just signal
            | maskInterrupt && signal /= NMI -> pure ()
            | otherwise -> handleInterruptSignal signal

handleInterruptSignal :: Interrupt -> CPU r ()
handleInterruptSignal signal = do
    pushAddrStack =<< getPC
    let mask = getFlagMask signal
    flag <-
        withCPUState $
            setFlag' BreakCommand (testBit mask 4)
                . setFlag' BreakCommand2 (testBit mask 5)
                . status
    pushByteStack $ unSR flag
    modifyCPUState $ modifyStatusRegister $ setFlag InterruptDisable
    setPC =<< readAddr (getVectorAddr signal) ()
