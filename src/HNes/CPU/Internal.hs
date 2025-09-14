{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HNes.CPU.Internal where

import Foreign
import HNes.Bus
import HNes.Memory

-- | Offset in the vram of the next instruction to execute
newtype ProgramCounter = PC {unPC :: MemoryAddr} deriving (Eq, Show, Num)

-- | Snapshot of the state of the CPU
data CPUState = MkCPUState
    { registerA :: {-# UNPACK #-} !Word8
    , status :: {-# UNPACK #-} !Word8
    , programCounter :: {-# UNPACK #-} !ProgramCounter
    }
    deriving (Eq, Show)

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

-- | Runs a program, returns the state of the CPU
runProgram :: Bus -> IO CPUState
runProgram = runProgramWithState newCPUState

-- | Runs a program, using a custom start state
runProgramWithState :: CPUState -> Bus -> IO CPUState
runProgramWithState state prog = unCPU interpret state prog $ \state' _ _ -> return state'

incrementPC :: CPU r ()
incrementPC = modifyCPUState $ \st -> st{programCounter = 1 + programCounter st}

-- | Read Word8 from memory, using the program counter as offset
readAtPC :: CPU r Word8
readAtPC = MkCPU $ \state bus cont ->
    readWord8 (unPC $ programCounter state) bus
        >>= cont state bus

-- | Get a brand new, clear CPU
newCPUState :: CPUState
newCPUState = MkCPUState 0 0 (PC 0)

-- | Interpretation loop of the program
interpret :: CPU r ()
interpret = do
    opCode <- readAtPC
    incrementPC
    if opCode == 0x00
        then return ()
        else go opCode >> interpret
  where
    go = \case
        0x00 -> pure () -- Redundant with the check
        _ -> fail "OP Code not implemented"
