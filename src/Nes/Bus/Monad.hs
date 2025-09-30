{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nes.Bus.Monad where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Ix
import Foreign
import Nes.Bus
import Nes.Bus.Constants
import Nes.Memory
import Nes.Rom
import Text.Printf

newtype BusM r a = MkBusM {unBusM :: Bus -> (Bus -> a -> IO r) -> IO r} deriving (Functor)

instance Applicative (BusM r) where
    pure a = MkBusM $ \bus cont -> cont bus a
    liftA2 f (MkBusM a) (MkBusM b) = MkBusM $ \bus cont ->
        a bus $ \bus' a' -> b bus' $ \bus'' b' -> cont bus'' (f a' b')

instance Monad (BusM r) where
    (MkBusM a) >>= next = MkBusM $ \bus cont ->
        a bus $ \bus' a' -> unBusM (next a') bus' $ \bus'' res -> cont bus'' res

instance MonadIO (BusM r) where
    liftIO io = MkBusM $ \bus cont -> io >>= cont bus

instance MonadFail (BusM r) where
    fail = liftIO . fail

runBusM :: Bus -> BusM (a, Bus) a -> IO (a, Bus)
runBusM bus f = unBusM f bus (\bus' a -> return (a, bus'))

withBus :: (Bus -> a) -> BusM r a
withBus f = MkBusM $ \bus cont -> cont bus (f bus)

tick :: Int -> BusM r ()
tick n = MkBusM $ \bus cont -> do
    replicateM_ n (cycleCallback bus)
    cont bus{cycles = cycles bus + fromIntegral n} ()

instance MemoryInterface () (BusM r) where
    readByte idx () = do
        checkBound idx
        translatedAddr <- translateReadAddr idx
        case translatedAddr of
            VRamAddr addr -> liftIO . readByte addr =<< withBus cpuVram
            PRGRomAddr addr -> do
                rom <- withBus cartridge
                readPrgRomAddr addr rom readByte
    readAddr idx () = do
        checkBound idx
        translatedAddr <- translateReadAddr idx
        case translatedAddr of
            VRamAddr addr -> liftIO . readAddr addr =<< withBus cpuVram
            PRGRomAddr addr -> do
                rom <- withBus cartridge
                readPrgRomAddr addr rom readAddr
    writeByte byte idx () = do
        checkBound idx
        translateWriteAddr idx $ \dest ->
            writeByte byte dest =<< withBus cpuVram
    writeAddr addr idx () = do
        checkBound idx
        translateWriteAddr idx $ \dest ->
            writeAddr addr dest =<< withBus cpuVram

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
