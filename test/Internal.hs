module Internal (runAndDump, runWithStateAndDump, runWithStateAndMemorySetupAndDump) where

import Data.Word
import Foreign
import GHC.ForeignPtr (unsafeWithForeignPtr)
import Nes.Bus
import Nes.CPU.Interpreter (runProgramWithState)
import Nes.CPU.State
import Nes.Memory
import Nes.Memory.Unsafe ()

-- | Runs a program and returns the state of the CPU at the end of the execution
runAndDump :: [Word8] -> IO CPUState
runAndDump = runWithStateAndDump newCPUState

runWithStateAndMemorySetupAndDump :: CPUState -> (Ptr () -> IO ()) -> [Word8] -> IO CPUState
runWithStateAndMemorySetupAndDump st memSetup program = do
    fptr <- newMemory
    loadProgramToMemory program fptr
    unsafeWithForeignPtr fptr memSetup
    st' <- resetPC fptr st
    runProgramWithState st' $ Bus fptr

runWithStateAndDump :: CPUState -> [Word8] -> IO CPUState
runWithStateAndDump st = runWithStateAndMemorySetupAndDump st (\_ -> return ())

resetPC :: MemoryPointer -> CPUState -> IO CPUState
resetPC fptr st = do
    pc <- readAddr 0xfffc fptr
    return $ st{programCounter = pc}

-- | Writes the given list of bytes at the given pointer
--
-- WARNING: The pointer should be allocated to 'memorySize'
loadProgramToMemory :: [Word8] -> MemoryPointer -> IO ()
loadProgramToMemory program fptr =
    unsafeWithForeignPtr
        fptr
        ( \ptr -> do
            loop program $ (`plusPtr` 0x8000) ptr
            writeAddr 0x8000 0xfffc ptr
        )
  where
    loop :: [Word8] -> Ptr Word8 -> IO ()
    loop [] _ = return ()
    loop (b : bs) ptr = do
        poke ptr b
        loop bs (ptr `plusPtr` 1)
