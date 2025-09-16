module Internal (withProgram, withState, withMemorySetup, withStateAndMemorySetup) where

import Data.Word
import Foreign
import GHC.ForeignPtr (unsafeWithForeignPtr)
import Nes.Bus
import Nes.CPU.Interpreter (runProgramWithState)
import Nes.CPU.State
import Nes.Memory
import Nes.Memory.Unsafe ()

withStateAndMemorySetup ::
    -- | Program
    [Word8] ->
    -- | Initial CPU State
    CPUState ->
    -- | Function that write to memory
    (Ptr a -> IO ()) ->
    -- | Continuation, run at the end of the program
    (CPUState -> Ptr a -> IO ()) ->
    IO ()
withStateAndMemorySetup program st memSetup post = do
    fptr <- newMemory
    loadProgramToMemory program fptr
    unsafeWithForeignPtr fptr $ memSetup . castPtr
    st' <- resetPC fptr st
    st'' <- runProgramWithState st' $ Bus fptr
    unsafeWithForeignPtr fptr $ post st'' . castPtr

-- | Runs a program and returns the state of the CPU at the end of the execution
withProgram :: [Word8] -> (CPUState -> IO ()) -> IO ()
withProgram program cont = withStateAndMemorySetup program newCPUState (\_ -> return ()) (\st _ -> cont st)

withState :: [Word8] -> CPUState -> (CPUState -> IO ()) -> IO ()
withState program st cont = withStateAndMemorySetup program st (\_ -> return ()) (\st' _ -> cont st')

withMemorySetup :: [Word8] -> (Ptr () -> IO ()) -> (CPUState -> Ptr () -> IO ()) -> IO ()
withMemorySetup program = withStateAndMemorySetup program newCPUState

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
