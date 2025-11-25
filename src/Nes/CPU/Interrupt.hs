module Nes.CPU.Interrupt (handleInterrupt) where

import Control.Monad
import Nes.APU.Monad (modifyDMCAndInterrupt)
import Nes.APU.State
import Nes.APU.State.DMC (DMC (sampleBufferAddr), loadSampleBuffer)
import Nes.Bus.Monad (liftAPU)
import Nes.Bus.State
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.FlagRegister
import Nes.Internal.MonadState
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
    maskInterrupt <- uses status $ getFlag InterruptDisable
    pendingSignal <- signalFromInterrupt <$> use cpuInterrupt
    case pendingSignal of
        Nothing -> pure ()
        Just signal
            | maskInterrupt && signal /= NMI && signal /= IRQ BRK -> pure ()
            | otherwise -> do
                pushAddrStack =<< use pc
                pushStatusRegister (signalShouldPushBFlag signal)
                status %= setFlag InterruptDisable
                (pc .=) =<< readAddr (signalVectorAddr signal) ()
    -- TODO Ugly, shouldn't be here
    when (pendingSignal == Just (IRQ DMC)) $ liftBus $ do
        sampleByteAddr <- sampleBufferAddr . _dmc <$> use apuState
        sample <- Nes.Memory.readByte sampleByteAddr ()
        liftAPU $ modifyDMCAndInterrupt $ loadSampleBuffer sample
    -- Cleanup state
    case pendingSignal of
        Nothing -> return ()
        Just NMI -> modify $ \s -> s{nmi = False}
        Just (IRQ _) -> modify $ \s -> s{irq = Nothing}
