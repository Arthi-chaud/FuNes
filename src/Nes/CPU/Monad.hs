{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nes.CPU.Monad (
    -- * Monad
    CPU (..),

    -- * Interracting with bus
    withBus,

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

import Control.Monad.IO.Class
import Data.Bits (Bits (shiftR), testBit)
import Nes.Bus (Bus (..))
import Nes.Bus.Constants
import Nes.Bus.Monad (BusM, runBusM)
import qualified Nes.Bus.Monad as BusM
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
    pure a = MkCPU $ \st prog cont -> cont st prog a
    (MkCPU f) <*> (MkCPU a) = MkCPU $ \st prog cont -> f st prog $
        \st' prog' f' -> a st' prog' $
            \st'' prog'' a' -> cont st'' prog'' $ f' a'

instance Monad (CPU r) where
    (MkCPU a) >>= next = MkCPU $ \st prog cont -> a st prog $
        \st' prog' a' -> unCPU (next a') st' prog' cont

instance MonadFail (CPU r) where
    fail s = MkCPU $ \_ _ _ -> fail s

instance MonadIO (CPU r) where
    liftIO io = MkCPU $ \st prog cont -> io >>= cont st prog

modifyCPUState :: (CPUState -> CPUState) -> CPU r ()
modifyCPUState f = MkCPU $ \st prog cont -> cont (f st) prog ()

withCPUState :: (CPUState -> a) -> CPU r a
withCPUState f = MkCPU $ \st prog cont -> cont st prog (f st)

getCycles :: CPU r Integer
getCycles = MkCPU $ \st bus cont -> cont st bus (cycles bus)

-- | Returns the value of the Program counter as an Addr
getPC :: CPU r Addr
getPC = withCPUState programCounter

setPC :: Addr -> CPU r ()
setPC addr = modifyCPUState $ \st -> st{programCounter = addr}

incrementPC :: CPU r ()
incrementPC = modifyCPUState $ \st -> st{programCounter = 1 + programCounter st}

-- | Read Word8 from memory, using the program counter as offset
readAtPC :: CPU r Byte
readAtPC = getPC >>= flip readByte ()

popStackByte :: CPU r Byte
popStackByte = do
    newRegS <- (+ 1) <$> withCPUState (getRegister S)
    modifyCPUState $ setRegister S newRegS
    readByte (stackAddr + byteToAddr newRegS) ()

popStackAddr :: CPU r Addr
popStackAddr = liftA2 bytesToAddr popStackByte popStackByte

pushByteStack :: Byte -> CPU r ()
pushByteStack byte = do
    regS <- withCPUState $ getRegister S
    writeByte byte (stackAddr + byteToAddr regS) ()
    modifyCPUState $ setRegister S (regS - 1)

pushAddrStack :: Addr -> CPU r ()
pushAddrStack addr = do
    let high = unsafeAddrToByte (shiftR addr 8)
        low = unsafeAddrToByte addr
    pushByteStack high
    pushByteStack low

withBus :: BusM (a, Bus) a -> CPU r a
withBus f = MkCPU $ \st bus cont -> do
    (res, bus') <- runBusM bus f
    cont st bus' res

-- | Unsafe action that provides access to Bus
--
-- When using it, ticks ARE NOT taken into account.
-- For testing purposes
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
    readByte n () = do
        res <- withBus (Nes.Memory.readByte n ())
        tickOnce
        return res

    readAddr n () = do
        res <- withBus (Nes.Memory.readAddr n ())
        tick 2
        return res

    writeByte byte dest () = do
        withBus (Nes.Memory.writeByte byte dest ())
        tickOnce

    writeAddr byte dest () = do
        withBus (Nes.Memory.writeAddr byte dest ())
        tick 2

tick :: Int -> CPU r ()
tick n = MkCPU $ \st bus cont -> do
    ((), newbus) <- runBusM bus $ BusM.tick n
    cont st newbus ()

tickOnce :: CPU r ()
tickOnce = Nes.CPU.Monad.tick 1
