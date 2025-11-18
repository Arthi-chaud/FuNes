module Nes.CPU.Interrupt (handleInterrupt) where

import Control.Monad
import Nes.APU.Monad (modifyAPUStateWithInterrupt)
import Nes.APU.State (APUState (dmc), modifyDMC')
import Nes.APU.State.DMC (DMC (sampleBufferAddr), loadSampleBuffer)
import Nes.Bus (Bus (apuState, cpuInterrupt))
import qualified Nes.Bus.Monad as BusM
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.FlagRegister
import Nes.Interrupt
import Nes.Memory

data Signal = NMI | IRQ IRQSource deriving (Eq)

{-# INLINE signalFromInterrupt #-}
signalFromInterrupt :: InterruptStatus -> Maybe Signal
signalFromInterrupt s
    | nmi s = Just NMI
    | otherwise = IRQ <$> irq s

signalShouldPushBFlag :: Signal -> Bool
signalShouldPushBFlag = \case
    NMI -> False
    IRQ BRK -> True
    IRQ _ -> False

signalVectorAddr :: Signal -> Addr
signalVectorAddr = \case
    NMI -> 0xfffa
    IRQ _ -> 0xfffe

handleInterrupt :: CPU r ()
handleInterrupt = do
    maskInterrupt <- withCPUState $ getFlag InterruptDisable . status
    pendingSignal <- signalFromInterrupt <$> withBusState cpuInterrupt
    case pendingSignal of
        Nothing -> pure ()
        Just signal
            | maskInterrupt && signal /= NMI && signal /= IRQ BRK -> pure ()
            | otherwise -> do
                pushAddrStack =<< getPC
                pushStatusRegister (signalShouldPushBFlag signal)
                modifyCPUState $ modifyStatusRegister $ setFlag InterruptDisable
                setPC =<< readAddr (signalVectorAddr signal) ()
    -- TODO Ugly, shouldn't be here
    when (pendingSignal == Just (IRQ DMC)) $ withBus $ do
        sampleByteAddr <- BusM.withBus $ sampleBufferAddr . dmc . apuState
        sample <- Nes.Memory.readByte sampleByteAddr ()
        BusM.withAPU $ modifyAPUStateWithInterrupt $ modifyDMC' $ loadSampleBuffer sample
    -- Cleanup state
    case pendingSignal of
        Nothing -> return ()
        Just NMI -> modifyInterruptStatus $ \s -> s{nmi = False}
        Just (IRQ _) -> modifyInterruptStatus $ \s -> s{irq = Nothing}
