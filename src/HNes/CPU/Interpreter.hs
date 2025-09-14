module HNes.CPU.Interpreter (runProgram, runProgramWithState, interpret) where

import HNes.Bus
import HNes.CPU.Instructions.LDA
import HNes.CPU.Monad
import HNes.CPU.State

-- TODO rename these 2 functions

-- | Runs the program that's on the memory (interfaced by the given 'Bus').
--
-- Returns the state of the CPU
runProgram :: Bus -> IO CPUState
runProgram = runProgramWithState newCPUState

-- | Same as 'runProgram', but using a given initial state
runProgramWithState :: CPUState -> Bus -> IO CPUState
runProgramWithState state prog = unCPU interpret state prog $ \state' _ _ -> return state'

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
        0xa9 -> lda
        0x00 -> pure () -- Redundant with the check
        _ -> fail "OP Code not implemented"
