module Nes.Bus where

import Control.Monad
import Control.Monad.IO.Class
import Data.Ix
import Foreign
import GHC.ForeignPtr (unsafeWithForeignPtr)
import GHC.Storable (writeWord8OffPtr)
import Nes.Memory
import Nes.Memory.Unsafe ()
import Text.Printf

-- Constants

ramRange :: (Addr, Addr)
ramRange = (0x0000, 0x1fff)

stackAddr :: Addr
stackAddr = 0x0100

stackReset :: Byte
stackReset = 0xfd

ppuRegisters :: (Addr, Addr)
ppuRegisters = (0x2000, 0x3fff)

-- | The address where to read the program's offset
programLocation :: Addr
programLocation = 0xfffc

-- | End of the program's in the memory
programEnd :: Addr
programEnd = memorySize

-- | Interface for the CPU that allows it to read/write to RAM
newtype Bus = Bus {memory :: MemoryPointer}

newBus :: IO Bus
newBus = do
    let vramSize = 2048
    fptr <- mallocForeignPtrBytes vramSize
    unsafeWithForeignPtr fptr $
        \ptr -> forM_ [0 .. vramSize] $ \idx -> writeWord8OffPtr ptr idx 0
    return $ Bus (castForeignPtr fptr)

instance MemoryInterface Bus where
    readByte idx (Bus fptr) = do
        checkBound idx
        addr <- translateReadAddr idx
        readByte addr fptr
    readAddr idx (Bus fptr) = do
        checkBound idx
        addr <- translateReadAddr idx
        readAddr addr fptr
    writeByte byte idx (Bus fptr) = do
        checkBound idx
        translateWriteAddr idx $ \dest ->
            writeByte byte dest fptr
    writeAddr addr idx (Bus fptr) = do
        checkBound idx
        translateWriteAddr idx $ \dest ->
            writeAddr addr dest fptr

checkBound :: (MonadFail m) => Addr -> m ()
checkBound idx = when (idx >= memorySize) $ fail "Out-of-bounds memory access"

translateReadAddr :: (MonadIO m) => Addr -> m Addr
translateReadAddr idx = case translateAddr idx of
    Just addr -> return addr
    _ -> do
        liftIO $ printf "Invalid virtual memory read at 0x%x" $ unAddr idx
        return 0

-- | Using CPS because in some case we noop
translateWriteAddr :: (MonadIO m) => Addr -> (Addr -> m ()) -> m ()
translateWriteAddr idx cont = case translateAddr idx of
    Just addr -> cont addr
    _ -> liftIO $ printf "Invalid virtual memory write at 0x%x" $ unAddr idx

translateAddr :: Addr -> Maybe Addr
translateAddr idx
    | inRange ramRange idx = Just $ idx .&. 0b11111111111
    | inRange ppuRegisters idx =
        let
            _ = idx .&. 0b0010000000000111
         in
            error "PPU is not supported yet" -- TODO
    | otherwise = Nothing
