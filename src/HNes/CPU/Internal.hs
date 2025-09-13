{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module HNes.CPU.Internal where

import Foreign
import HNes.Internal

-- | Snapshot of the state of the CPU
data CPUState = MkCPUState
    { registerA :: {-# UNPACK #-} !Word8
    , status :: {-# UNPACK #-} !Word8
    , programCounter :: {-# UNPACK #-} !ProgramCounter
    }
    deriving (Eq, Show)

newtype CPU r m a = MkCPU
    { unCPU ::
        CPUState ->
        Program ->
        (CPUState -> Program -> a -> m r) ->
        m r
    }
    deriving (Functor)

instance (Monad m) => Applicative (CPU r m) where
    pure a = MkCPU $ \st prog cont -> cont st prog a
    (MkCPU f) <*> (MkCPU a) = MkCPU $ \st prog cont -> f st prog $
        \st' prog' f' -> a st' prog' $
            \st'' prog'' a' -> cont st'' prog'' $ f' a'

instance (Monad m) => Monad (CPU r m) where
    (MkCPU a) >>= next = MkCPU $ \st prog cont -> a st prog $
        \st' prog' a' -> unCPU (next a') st' prog' cont

instance (MonadFail m) => MonadFail (CPU r m) where
    fail s = MkCPU $ \_ _ _ -> fail s

modifyCPUState :: (CPUState -> CPUState) -> CPU r m ()
modifyCPUState f = MkCPU $ \st prog cont -> cont (f st) prog ()

incrementPC :: CPU r m ()
incrementPC = modifyCPUState $ \st -> st{programCounter = 1 + programCounter st}

-- | Get a brand new, clear CPU
newCPUState :: CPUState
newCPUState = MkCPUState 0 0 (PC 0)

-- | Runs a program, returns the state of the CPU
runProgram :: (MonadFail m) => Program -> m CPUState
runProgram = runProgramWithState newCPUState

-- | Runs a program, using a custom start state
runProgramWithState :: (MonadFail m) => CPUState -> Program -> m CPUState
runProgramWithState state prog = unCPU interpret state prog $ \state' _ _ -> return state'

-- | Interpretation loop of the program
interpret :: (MonadFail m) => CPU r m ()
interpret = do
    opCode <-
        MkCPU
            (\state prog cont -> cont state prog $ readByteOffProgram (programCounter state) prog)
    incrementPC
    if opCode == 0x00
        then return ()
        else go opCode >> interpret
  where
    go = \case
        0x00 -> pure () -- Redundant with the check
        -- TODO use fail instead
        _ -> fail "OP Code not implemented"
