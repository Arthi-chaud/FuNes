{-# LANGUAGE FlexibleInstances #-}

module Nes.CPU.Monad (
    -- * Monad
    CPU (..),

    -- * State
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

    -- * Bus
    liftBus,
    unsafeLiftBus,

    -- * Re-exports
    module Nes.Internal.MonadState,
) where

import Control.Monad.IO.Class
import Data.Bits (Bits (shiftR))
import Nes.Bus (Bus (..))
import Nes.Bus.Constants
import Nes.Bus.Monad (BusM, runBusM)
import qualified Nes.Bus.Monad as BusM
import Nes.CPU.State
import Nes.FlagRegister
import Nes.Internal.MonadState
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

instance MonadState CPUState (CPU r) where
    {-# INLINE set #-}
    set st' = MkCPU $ \_ bus cont -> cont st' bus ()
    {-# INLINE get #-}
    get = MkCPU $ \st bus cont -> cont st bus st

instance MonadState Bus (CPU r) where
    {-# INLINE set #-}
    set bus' = MkCPU $ \st _ cont -> cont st bus' ()
    {-# INLINE get #-}
    get = MkCPU $ \st bus cont -> cont st bus bus

instance MonadState InterruptStatus (CPU r) where
    {-# INLINE set #-}
    set is = MkCPU $ \st bus cont -> cont st bus{cpuInterrupt = is} ()
    {-# INLINE get #-}
    get = MkCPU $ \st bus cont -> cont st bus (cpuInterrupt bus)

liftBus :: BusM (a, Bus) a -> CPU r a
liftBus f = MkCPU $ \st bus cont -> do
    (res, bus') <- runBusM bus f
    cont st bus' res

-- | Unsafe action that provides access to Bus
--
-- When using it, ticks ARE NOT taken into account.
-- For testing purposes
{-# INLINE unsafeLiftBus #-}
unsafeLiftBus :: BusM (a, Bus) a -> CPU r a
unsafeLiftBus f = MkCPU $ \st bus cont -> do
    (res, _) <- runBusM bus f
    cont st bus res

{-# INLINE tick #-}
tick :: Int -> CPU r ()
tick = liftBus . BusM.tick

{-# INLINE tickOnce #-}
tickOnce :: CPU r ()
tickOnce = Nes.CPU.Monad.tick 1

instance MemoryInterface () (CPU r) where
    {-# INLINE readByte #-}
    readByte n () = do
        res <- liftBus (Nes.Memory.readByte n ())
        tickOnce
        return res

    {-# INLINE readAddr #-}
    readAddr n () = do
        res <- liftBus (Nes.Memory.readAddr n ())
        tick 2
        return res

    {-# INLINE writeByte #-}
    writeByte byte dest () = do
        liftBus (Nes.Memory.writeByte byte dest ())
        tickOnce

    {-# INLINE writeAddr #-}
    writeAddr byte dest () = do
        liftBus (Nes.Memory.writeAddr byte dest ())
        tick 2

-- | Returns the value of the Program counter as an Addr
{-# INLINE getPC #-}
getPC :: CPU r Addr
getPC = gets programCounter

setPC :: Addr -> CPU r ()
setPC addr = modify $ \st -> st{programCounter = addr}

{-# INLINE incrementPC #-}
incrementPC :: CPU r ()
incrementPC = modify $ \st -> st{programCounter = 1 + programCounter st}

-- | Read Word8 from memory, using the program counter as offset
{-# INLINE readAtPC #-}
readAtPC :: CPU r Byte
readAtPC = getPC >>= flip readByte ()

popStackByte :: CPU r Byte
popStackByte = do
    newRegS <- (+ 1) <$> gets (getRegister S)
    modify $ setRegister S newRegS
    readByte (stackAddr + byteToAddr newRegS) ()

{-# INLINE popStackAddr #-}
popStackAddr :: CPU r Addr
popStackAddr = liftA2 bytesToAddr popStackByte popStackByte

pushByteStack :: Byte -> CPU r ()
pushByteStack byte = do
    regS <- gets $ getRegister S
    writeByte byte (stackAddr + byteToAddr regS) ()
    modify $ setRegister S (regS - 1)

-- | If the argument is True, the pushed value will have the B Flag set
pushStatusRegister :: Bool -> CPU r ()
pushStatusRegister b = do
    s <- gets status
    let value = unSR $ setFlag Unusued $ setFlag' BFlag b s
    pushByteStack value

-- | Pops value on the stack, clear BFlag and sets the results value as status register
popStatusRegister :: CPU r ()
popStatusRegister = do
    value <- fromByte <$> popStackByte
    -- TODO Breaks Nestest
    let s = clearFlag Unusued $ clearFlag BFlag value
    modify $ modifyStatusRegister $ const s

{-# INLINE pushAddrStack #-}
pushAddrStack :: Addr -> CPU r ()
pushAddrStack addr = do
    let high = unsafeAddrToByte (shiftR addr 8)
        low = unsafeAddrToByte addr
    pushByteStack high
    pushByteStack low

-- | Resets the state of the CPU
reset :: CPU r ()
reset = do
    set newCPUState
    pc <- readAddr 0xfffc ()
    set newCPUState{programCounter = pc}
