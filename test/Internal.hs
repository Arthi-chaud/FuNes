module Internal (runAndDump, runWithStateAndDump) where

import Data.Word
import Foreign
import GHC.ForeignPtr (unsafeWithForeignPtr)
import HNes.Bus
import HNes.CPU.Interpreter (runProgramWithState)
import HNes.CPU.State
import HNes.Memory

-- | Runs a program and returns the state of the CPU at the end of the execution
runAndDump :: [Word8] -> IO CPUState
runAndDump = runWithStateAndDump newCPUState

runWithStateAndDump :: CPUState -> [Word8] -> IO CPUState
runWithStateAndDump st program = do
    fptr <- newMemory
    writeProgramToMemory program fptr
    runProgramWithState st $ Bus fptr

-- | Writes the given list of bytes at the given pointer
--
-- WARNING: The pointer should be allocated to 'memorySize'
writeProgramToMemory :: [Word8] -> MemoryPointer -> IO ()
writeProgramToMemory program fptr = unsafeWithForeignPtr fptr (loop program . castPtr)
  where
    loop :: [Word8] -> Ptr Word8 -> IO ()
    loop [] _ = return ()
    loop (b : bs) ptr = do
        poke ptr b
        loop bs (ptr `plusPtr` 1)
