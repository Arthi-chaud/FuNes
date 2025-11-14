{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nes.CPU.Monad (
    -- * Monad
    CPU (..),

    -- * Interracting with bus
    withBus,
    withBusState,
    setSideEffect,

    -- * State
    modifyCPUState,
    withCPUState,
    getCycles,
    reset,
    --- * PC
    getPC,
    setPC,
    incrementPC,
    readAtPC,
    --- * Ticks
    tick,
    tickOnce,

    -- * Stack
    popStackAddr,
    popStackByte,
    pushAddrStack,
    pushByteStack,

    -- * Interruption
    interrupt,

    -- * Unsafe
    unsafeWithBus,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bits (Bits (shiftR), testBit)
import Nes.APU.Monad (modifyAPUState)
import Nes.APU.State (APUState (dmc), modifyDMC)
import Nes.APU.State.DMC (DMC (sampleBuffer, sampleBufferAddr))
import Nes.Bus (Bus (..))
import Nes.Bus.Constants
import Nes.Bus.Monad (BusM, runBusM)
import qualified Nes.Bus.Monad as BusM
import Nes.Bus.SideEffect
import Nes.CPU.State
import Nes.FlagRegister
import Nes.Interrupt
import Nes.Memory

-- | Note: we use IO because it is likely to read/write from/to memory, which is not pure
newtype CPU r a = MkCPU
    { unCPU ::
        CPUState ->
        Bus ->
        (CPUState -> Bus -> a -> IO r) ->
        IO r
    }
    deriving (Functor)

instance Applicative (CPU r) where
    {-# INLINE pure #-}
    pure a = MkCPU $ \st bus cont -> cont st bus a
    {-# INLINE (<*>) #-}
    (MkCPU f) <*> (MkCPU a) = MkCPU $ \st bus cont -> f st bus $
        \st' prog' f' -> a st' prog' $
            \st'' prog'' a' -> cont st'' prog'' $ f' a'

instance Monad (CPU r) where
    {-# INLINE (>>=) #-}
    (MkCPU a) >>= next = MkCPU $ \st bus cont -> a st bus $
        \st' bus' a' -> unCPU (next a') st' bus' cont

instance MonadFail (CPU r) where
    {-# INLINE fail #-}
    fail s = MkCPU $ \_ _ _ -> fail s

instance MonadIO (CPU r) where
    {-# INLINE liftIO #-}
    liftIO io = MkCPU $ \st bus cont -> io >>= cont st bus

{-# INLINE modifyCPUState #-}
modifyCPUState :: (CPUState -> CPUState) -> CPU r ()
modifyCPUState f = MkCPU $ \st bus cont -> cont (f st) bus ()

{-# INLINE withCPUState #-}
withCPUState :: (CPUState -> a) -> CPU r a
withCPUState f = MkCPU $ \st bus cont -> cont st bus (f st)

withBusState :: (Bus -> a) -> CPU r a
withBusState f = MkCPU $ \st bus cont -> cont st bus (f bus)

{-# INLINE getCycles #-}
getCycles :: CPU r Integer
getCycles = withBusState cycles

setSideEffect :: (CPUSideEffect -> CPUSideEffect) -> CPU r ()
setSideEffect f = MkCPU $ \st bus cont -> cont st bus{cpuSideEffect = f $ cpuSideEffect bus} ()

{-# INLINE getPC #-}

-- | Returns the value of the Program counter as an Addr
getPC :: CPU r Addr
getPC = withCPUState programCounter

setPC :: Addr -> CPU r ()
setPC addr = modifyCPUState $ \st -> st{programCounter = addr}

{-# INLINE incrementPC #-}
incrementPC :: CPU r ()
incrementPC = modifyCPUState $ \st -> st{programCounter = 1 + programCounter st}

-- | Read Word8 from memory, using the program counter as offset
{-# INLINE readAtPC #-}
readAtPC :: CPU r Byte
readAtPC = getPC >>= flip readByte ()

popStackByte :: CPU r Byte
popStackByte = do
    newRegS <- (+ 1) <$> withCPUState (getRegister S)
    modifyCPUState $ setRegister S newRegS
    readByte (stackAddr + byteToAddr newRegS) ()

{-# INLINE popStackAddr #-}
popStackAddr :: CPU r Addr
popStackAddr = liftA2 bytesToAddr popStackByte popStackByte

pushByteStack :: Byte -> CPU r ()
pushByteStack byte = do
    regS <- withCPUState $ getRegister S
    writeByte byte (stackAddr + byteToAddr regS) ()
    modifyCPUState $ setRegister S (regS - 1)

{-# INLINE pushAddrStack #-}
pushAddrStack :: Addr -> CPU r ()
pushAddrStack addr = do
    let high = unsafeAddrToByte (shiftR addr 8)
        low = unsafeAddrToByte addr
    pushByteStack high
    pushByteStack low

{-# INLINE withBus #-}
withBus :: BusM (a, Bus) a -> CPU r a
withBus f = do
    res <- MkCPU $ \st bus cont -> do
        (res, bus') <- runBusM bus f
        cont st bus' res
    handleSideEffect
    return res

-- | Unsafe action that provides access to Bus
--
-- When using it, ticks ARE NOT taken into account.
-- For testing purposes
{-# INLINE unsafeWithBus #-}
unsafeWithBus :: BusM (a, Bus) a -> CPU r a
unsafeWithBus f = MkCPU $ \st bus cont -> do
    (res, _) <- runBusM bus f
    cont st bus res

-- | Resets the state of the CPU
reset :: CPU r ()
reset = do
    modifyCPUState $ const newCPUState
    pc <- readAddr 0xfffc ()
    modifyCPUState (const $ newCPUState{programCounter = pc})

interrupt :: Interrupt -> CPU r ()
interrupt signal = do
    pushAddrStack =<< getPC
    let mask = getFlagMask signal
    flag <-
        withCPUState $
            setFlag' BreakCommand (testBit mask 4)
                . setFlag' BreakCommand2 (testBit mask 5)
                . status
    pushByteStack $ unSR flag
    modifyCPUState $ modifyStatusRegister $ setFlag InterruptDisable
    withBus $ BusM.tick (getCPUCycles signal)
    setPC =<< readAddr (getVectorAddr signal) ()

instance MemoryInterface () (CPU r) where
    {-# INLINE readByte #-}
    readByte n () = do
        res <- withBus (Nes.Memory.readByte n ())
        tickOnce
        return res

    {-# INLINE readAddr #-}
    readAddr n () = do
        res <- withBus (Nes.Memory.readAddr n ())
        tick 2
        return res

    {-# INLINE writeByte #-}
    writeByte byte dest () = do
        withBus (Nes.Memory.writeByte byte dest ())
        tickOnce

    {-# INLINE writeAddr #-}
    writeAddr byte dest () = do
        withBus (Nes.Memory.writeAddr byte dest ())
        tick 2

{-# INLINE tick #-}
tick :: Int -> CPU r ()
tick = withBus . BusM.tick

{-# INLINE tickOnce #-}
tickOnce :: CPU r ()
tickOnce = Nes.CPU.Monad.tick 1

handleSideEffect :: CPU r ()
handleSideEffect = do
    hasDMCDMA <- withBusState $ getFlag DMCDMA . cpuSideEffect
    when hasDMCDMA $ withBus $ do
        sampleByteAddr <- BusM.withBus $ sampleBufferAddr . dmc . apuState
        sample <- Nes.Memory.readByte sampleByteAddr ()
        BusM.withAPU $ modifyAPUState $ modifyDMC $ \d -> d{sampleBuffer = Just sample}
        BusM.modifyBus $ \b -> b{cpuSideEffect = clearFlag DMCDMA (cpuSideEffect b)}
