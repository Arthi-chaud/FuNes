module HNes.CPU.Internal where

import Control.Monad.State hiding (state)
import Foreign
import HNes.Internal

-- | Snapshot of the state of the CPU
data CPUState = MkCPUState
    { registerA :: {-# UNPACK #-} !Word8
    , status :: {-# UNPACK #-} !Word8
    , programCounter :: {-# UNPACK #-} !ProgramCounter
    }
    deriving (Eq, Show)

type CPU a =
    -- Note: Instead of using StateT + Reader
    -- We limit the wrapping overhead by also threading the program in the state
    --
    -- TODO Optimise: Use CPS
    State (CPUState, Program) a

incrementPC :: CPU ()
incrementPC = modify' $ \(st, prog) -> (st{programCounter = 1 + programCounter st}, prog)

-- | Get a brand new, clear CPU
newCPUState :: CPUState
newCPUState = MkCPUState 0 0 (PC 0)

-- | Runs a program, returns the state of the CPU
runProgram :: Program -> CPUState
runProgram = runProgramWithState newCPUState

-- | Runs a program, using a custom start state
runProgramWithState :: CPUState -> Program -> CPUState
runProgramWithState state prog = fst $ execState interpret (state, prog)

-- | Interpretation loop of the program
interpret :: CPU ()
interpret = do
    opCode <- gets (\(state, prog) -> readByteOffProgram (programCounter state) prog)
    incrementPC
    if opCode == 0x00
        then return ()
        else case opCode of
            0x00 -> pure () -- Redundant with the check
            _ -> error "OP Code not implemented"
