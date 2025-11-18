module Nes.CPU.Interrupt (handleInterrupt) where

import Nes.CPU.Monad
import Nes.CPU.State
import Nes.FlagRegister
import Nes.Interrupt (IRQSource (..), Interrupt (..), getVectorAddr, pushesBFlag)
import Nes.Memory

handleInterrupt :: CPU r ()
handleInterrupt = do
    maskInterrupt <- withCPUState $ getFlag InterruptDisable . status
    pendingInterrupt <- popInterrupt
    case pendingInterrupt of
        Nothing -> pure ()
        Just signal
            | maskInterrupt && signal /= NMI && signal /= IRQ BRK -> pure ()
            | otherwise -> handleInterruptSignal signal

handleInterruptSignal :: Interrupt -> CPU r ()
handleInterruptSignal signal = do
    pushAddrStack =<< getPC
    pushStatusRegister (pushesBFlag signal)
    modifyCPUState $ modifyStatusRegister $ setFlag InterruptDisable
    setPC =<< readAddr (getVectorAddr signal) ()
