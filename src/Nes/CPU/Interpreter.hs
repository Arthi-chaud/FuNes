module Nes.CPU.Interpreter (runProgram, runProgramWithState, interpret) where

import Data.Map
import Nes.Bus
import Nes.CPU.Instructions.Map
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory
import Text.Printf

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
    go opcode = case Data.Map.lookup opcode opcodeMap of
        Just (_, f, mode) -> f mode
        Nothing -> fail $ printf "OP Code not implemented: 0x%x" (unByte opcode)
