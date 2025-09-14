{-# LANGUAGE DeriveFunctor #-}

module HNes.CPU.Monad where

import Data.Word
import HNes.Bus
import HNes.CPU.State
import HNes.Memory

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

modifyCPUState :: (CPUState -> CPUState) -> CPU r ()
modifyCPUState f = MkCPU $ \st prog cont -> cont (f st) prog ()

withCPUState :: (CPUState -> a) -> CPU r a
withCPUState f = MkCPU $ \st prog cont -> cont st prog (f st)

incrementPC :: CPU r ()
incrementPC = modifyCPUState $ \st -> st{programCounter = 1 + programCounter st}

-- | Read Word8 from memory, using the program counter as offset
readAtPC :: CPU r Word8
readAtPC = MkCPU $ \state bus cont ->
    readWord8 (unPC $ programCounter state) bus
        >>= cont state bus

setRegisterA :: Word8 -> CPU r ()
setRegisterA byte = modifyCPUState (\st -> st{registerA = byte})

getRegisterA :: CPU r Word8
getRegisterA = withCPUState registerA

setStatusFlag :: Flag -> CPU r ()
setStatusFlag flag = modifyCPUState (setStatusFlagPure flag)

clearStatusFlag :: Flag -> CPU r ()
clearStatusFlag flag = modifyCPUState (clearStatusFlagPure flag)

getStatusFlag :: Flag -> CPU r Bool
getStatusFlag flag = withCPUState (getStatusFlagPure flag)
