module Nes.CPU.Interrupt (handleInterrupt) where

import Control.Monad
import Nes.APU.Monad (modifyAPUStateWithInterrupt)
import Nes.APU.State (APUState (dmc), modifyDMC')
import Nes.APU.State.DMC (DMC (sampleBufferAddr), loadSampleBuffer)
import Nes.Bus (Bus (apuState))
import qualified Nes.Bus.Monad as BusM
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
    -- Ugly, shouldn't be here
    when (signal == IRQ DMC) $ withBus $ do
        sampleByteAddr <- BusM.withBus $ sampleBufferAddr . dmc . apuState
        sample <- Nes.Memory.readByte sampleByteAddr ()
        BusM.withAPU $ modifyAPUStateWithInterrupt $ modifyDMC' $ loadSampleBuffer sample
