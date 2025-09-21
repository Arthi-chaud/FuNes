module Nes.CPU.Interpreter (
    runProgram,
    interpret,
    interpretWithCallback,
) where

import Data.Map
import Nes.Bus
import Nes.CPU.Instructions.Map
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory
import Text.Printf

-- | Runs the program that's on the memory (interfaced by the given 'Bus') usingthe given CPU state.
--
-- The thrid argument is a callback run at each loop
--
-- Returns the state of the CPU
runProgram :: CPUState -> Bus -> CPU CPUState () -> IO CPUState
runProgram state prog callback = unCPU (interpretWithCallback callback) state prog $ \state' _ _ -> return state'

interpret :: CPU r ()
interpret = interpretWithCallback $ pure ()

-- | Interpretation loop of the program
interpretWithCallback :: CPU r () -> CPU r ()
interpretWithCallback callback = do
    callback
    opCode <- readAtPC
    incrementPC
    if opCode == 0x00
        then return ()
        else go opCode >> interpret
  where
    go opcode = case Data.Map.lookup opcode opcodeMap of
        Just (_, f, mode) -> f mode
        Nothing -> fail $ printf "OP Code not implemented: 0x%x\n" (unByte opcode)
