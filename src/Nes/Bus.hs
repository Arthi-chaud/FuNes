{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Nes.Bus where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Functor (($>))
import Data.Ix
import Foreign
import Nes.Internal
import Nes.Memory
import Nes.Memory.Unsafe ()
import Nes.PPU.Pointers (PPUPointers, newPPUPointers)
import Nes.PPU.State (PPUState, newPPUState)
import Nes.Rom (Rom (..))
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

prgRomRange :: (Addr, Addr)
prgRomRange = (0x8000, 0xffff)

-- | Interface for the CPU that allows it to read/write to RAM
data Bus = Bus
    { memory :: MemoryPointer
    -- ^ Pointer to writeable memory
    , cartridge :: Rom
    -- ^ Read-only memory, see 'Rom'
    , cycles :: Integer
    -- ^ The number of ellapsed cycles
    , cycleCallback :: IO ()
    -- ^ The function executed on every tick (e.g. sleep)
    , ppuState :: PPUState
    -- ^ The state of the PPU
    , ppuPointers :: PPUPointers
    -- ^ Memory dedicated to PPU
    }

newBus :: Rom -> IO Bus
newBus rom_ = do
    fptr <- callocVram
    ppuPtrs <- newPPUPointers (chrRom rom_)
    let ppuSt = newPPUState (mirroring rom_)
    return $ Bus (castForeignPtr fptr) rom_ 0 (pure ()) ppuSt ppuPtrs
  where
    vramSize = 2048
    callocVram = callocForeignPtr vramSize

tick :: Int -> Bus -> IO Bus
tick n bus = replicateM_ n (cycleCallback bus) $> bus{cycles = cycles bus + fromIntegral n}

instance (MonadFail m, MonadIO m, MemoryInterface MemoryPointer m) => MemoryInterface Bus m where
    readByte idx (Bus fptr rom _ _ _ _) = do
        checkBound idx
        translatedAddr <- translateReadAddr idx
        case translatedAddr of
            VRamAddr addr -> readByte addr fptr
            PRGRomAddr addr -> readPrgRomAddr addr rom readByte
    readAddr idx (Bus fptr rom _ _ _ _) = do
        checkBound idx
        translatedAddr <- translateReadAddr idx
        case translatedAddr of
            VRamAddr addr -> readAddr addr fptr
            PRGRomAddr addr -> readPrgRomAddr addr rom readAddr
    writeByte byte idx (Bus fptr _ _ _ _ _) = do
        checkBound idx
        translateWriteAddr idx $ \dest ->
            writeByte byte dest fptr
    writeAddr addr idx (Bus fptr _ _ _ _ _) = do
        checkBound idx
        translateWriteAddr idx $ \dest ->
            writeAddr addr dest fptr

checkBound :: (MonadFail m) => Addr -> m ()
checkBound idx = when (idx >= memorySize) $ fail "Out-of-bounds memory access"

data TranslatedAddr = VRamAddr Addr | PRGRomAddr Addr

translateReadAddr :: (MonadIO m) => Addr -> m TranslatedAddr
translateReadAddr idx = case translateAddr idx of
    Just addr -> return addr
    _ -> do
        liftIO $ printf "Invalid virtual memory read at 0x%x" $ unAddr idx
        return (VRamAddr 0)

-- | Using CPS because in some case we noop
translateWriteAddr :: (MonadIO m, MonadFail m) => Addr -> (Addr -> m ()) -> m ()
translateWriteAddr idx cont = case translateAddr idx of
    Just (VRamAddr addr) -> cont addr
    Just (PRGRomAddr _) -> fail "Attempt to write to Cartridge ROM space"
    _ -> liftIO $ printf "Invalid virtual memory write at 0x%x" $ unAddr idx

translateAddr :: Addr -> Maybe TranslatedAddr
translateAddr idx
    | inRange ramRange idx = Just $ VRamAddr $ idx .&. 0b11111111111
    | inRange prgRomRange idx = Just $ PRGRomAddr (idx - fst prgRomRange)
    | inRange ppuRegisters idx =
        let
            _ = idx .&. 0b0010000000000111
         in
            -- TODO
            -- error "PPU is not supported yet"
            return $ VRamAddr 0
    | otherwise = Nothing

-- | The continuation will be called with the translated addr to use on the PRG Rom
-- No bound check are necessary
readPrgRomAddr :: (MonadFail m) => Addr -> Rom -> (Addr -> ForeignPtr Word8 -> m a) -> m a
readPrgRomAddr addr rom cont = do
    let prgRomSize = BS.length (prgRom rom)
        translatedAddr =
            if prgRomSize == 0x4000 && addr >= 0x4000
                then Addr $ unAddr addr `mod` 0x4000
                else addr
    when (addrToInt translatedAddr > prgRomSize) $ fail "Out-of-bound access in ROM"
    let ptr = let (BS.BS ptr' _) = prgRom rom in ptr'
    cont translatedAddr ptr
