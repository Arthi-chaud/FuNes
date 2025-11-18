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

    -- * Status register
    popStatusRegister,
    pushStatusRegister,

    -- * Interrupt
    modifyInterruptStatus,

    -- * Unsafe
    unsafeWithBus,
) where

import Control.Monad.IO.Class
import Data.Bits (Bits (shiftR))
import Nes.Bus (Bus (..))
import qualified Nes.Bus
import Nes.Bus.Constants
import Nes.Bus.Monad (BusM, modifyBus, runBusM)
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

-- | If the argument is True, the pushed value will have the B Flag set
pushStatusRegister :: Bool -> CPU r ()
pushStatusRegister b = do
    s <- withCPUState status
    let value = unSR $ setFlag Unusued $ setFlag' BFlag b s
    pushByteStack value

-- | Pops value on the stack, clear BFlag and sets the results value as status register
popStatusRegister :: CPU r ()
popStatusRegister = do
    value <- fromByte <$> popStackByte
    -- TODO Breaks Nestest
    let s = clearFlag Unusued $ clearFlag BFlag value
    modifyCPUState $ modifyStatusRegister $ const s

{-# INLINE pushAddrStack #-}
pushAddrStack :: Addr -> CPU r ()
pushAddrStack addr = do
    let high = unsafeAddrToByte (shiftR addr 8)
        low = unsafeAddrToByte addr
    pushByteStack high
    pushByteStack low

{-# INLINE withBus #-}
withBus :: BusM (a, Bus) a -> CPU r a
withBus f = MkCPU $ \st bus cont -> do
    (res, bus') <- runBusM bus f
    cont st bus' res

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

modifyInterruptStatus :: (InterruptStatus -> InterruptStatus) -> CPU r ()
modifyInterruptStatus = withBus . modifyBus . Nes.Bus.modifyInterruptStatus

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
