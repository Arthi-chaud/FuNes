{-# LANGUAGE DeriveFunctor #-}

module Nes.CPU.Monad where

import Control.Monad.IO.Class
import Data.Word
import Nes.Bus
import Nes.CPU.State
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

-- | Returns the value of the Program counter as an Addr
getPCAsAddr :: CPU r Addr
getPCAsAddr = withCPUState $ unPC . programCounter

incrementPC :: CPU r ()
incrementPC = modifyCPUState $ \st -> st{programCounter = 1 + programCounter st}

-- | Read Word8 from memory, using the program counter as offset
readAtPC :: CPU r Byte
readAtPC = getPCAsAddr >>= withBus . readByte

setRegister :: Register -> Byte -> CPU r ()
setRegister reg byte = modifyCPUState $ setRegisterPure reg byte

getRegister :: Register -> CPU r Byte
getRegister = withCPUState . getRegisterPure

setStatusFlag :: Flag -> CPU r ()
setStatusFlag flag = modifyCPUState (setStatusFlagPure flag)

clearStatusFlag :: Flag -> CPU r ()
clearStatusFlag flag = modifyCPUState (clearStatusFlagPure flag)

getStatusFlag :: Flag -> CPU r Bool
getStatusFlag flag = withCPUState (getStatusFlagPure flag)

-- | Run a memory access operation
withBus :: (Bus -> IO a) -> CPU r a
withBus f = MkCPU $ \st bus cont -> do
    res <- f bus
    cont st bus res

-- | Resets the state of the CPU
reset :: CPU r ()
reset = do
    pc <- withBus $ readAddr 0xfffc
    modifyCPUState
        ( \st ->
            st
                { registerA = 0
                , registerX = 0
                , status = 0
                , programCounter = PC pc
                }
        )
