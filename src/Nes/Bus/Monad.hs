module Nes.Bus.Monad (Bus (..), runBus, tick, liftPPU, liftAPU, liftController) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Functor (($>))
import Data.Ix
import Foreign
import Nes.APU.BusInterface
import Nes.APU.Monad
import Nes.APU.State
import qualified Nes.APU.Tick as APU
import Nes.Bus.Constants
import Nes.Bus.State
import Nes.Controller
import Nes.FlagRegister (clearFlag)
import Nes.Internal.MonadState
import Nes.Interrupt (InterruptStatus)
import Nes.Memory
import Nes.PPU.Constants (oamDataSize)
import Nes.PPU.Monad hiding (modifyPPUState, tick)
import qualified Nes.PPU.Monad as PPUM
import Nes.PPU.State hiding (cycles)
import Nes.Rom

newtype Bus r a = MkBus {unBus :: BusState -> (BusState -> a -> IO r) -> IO r} deriving (Functor)

instance Applicative (Bus r) where
    {-# INLINE pure #-}
    pure a = MkBus $ \bus cont -> cont bus a

    {-# INLINE liftA2 #-}
    liftA2 f (MkBus a) (MkBus b) = MkBus $ \bus cont ->
        a bus $ \bus' a' -> b bus' $ \bus'' b' -> cont bus'' (f a' b')

instance Monad (Bus r) where
    {-# INLINE (>>=) #-}
    (MkBus a) >>= next = MkBus $ \bus cont ->
        a bus $ \bus' a' -> unBus (next a') bus' $ \bus'' res -> cont bus'' res

instance MonadIO (Bus r) where
    {-# INLINE liftIO #-}
    liftIO io = MkBus $ \bus cont -> io >>= cont bus

instance MonadFail (Bus r) where
    {-# INLINE fail #-}
    fail = liftIO . fail

instance MonadState BusState (Bus r) where
    {-# INLINE set #-}
    set bus' = MkBus $ \_ cont -> cont bus' ()
    {-# INLINE get #-}
    get = MkBus $ \bus cont -> cont bus bus

{-# INLINE runBus #-}
runBus :: BusState -> Bus (a, BusState) a -> IO (a, BusState)
runBus bus f = unBus f bus (\bus' a -> return (a, bus'))

{-# INLINE liftPPU #-}
liftPPU :: PPU (a, PPUState) a -> Bus r a
liftPPU f = MkBus $ \bus cont -> do
    (res, ppuSt) <- runPPU (ppuState bus) (ppuPointers bus) (cartridge bus) f
    cont (bus{ppuState = ppuSt}) res

{-# INLINE liftAPU #-}
liftAPU :: APU (a, APUState, InterruptStatus) a -> Bus r a
liftAPU f = MkBus $ \bus cont -> do
    (!res, !apuSt, !interr) <- runAPU (apuState bus) (cpuInterrupt bus) f
    cont (bus{apuState = apuSt, cpuInterrupt = interr}) res

{-# INLINE liftController #-}
liftController :: Controller (a, ControllerState) a -> Bus r a
liftController f = MkBus $ \bus cont ->
    let
        (res, controller') = runController f (controller bus)
     in
        cont (bus{controller = controller'}) res

tick :: Int -> Bus r ()
tick n = MkBus $ \bus cont -> do
    let unsleptCycles_ = n + unsleptCycles bus
    (newLastSleepTime, newUnsleptCycles) <-
        cycleCallback bus (lastSleepTime bus) unsleptCycles_
    (isNewFrame, ppuSt) <- runPPU (ppuState bus) (ppuPointers bus) (cartridge bus) $ do
        before <- withPPUState nmiInterrupt
        _ <- PPUM.tick (n * 3)
        after <- withPPUState nmiInterrupt
        return (not before && after)
    ((), !apuSt, !interr) <- runAPU (apuState bus) (cpuInterrupt bus) $ APU.tick (odd (Nes.Bus.State.cycles bus)) n
    let bus' =
            bus
                { unsleptCycles = newUnsleptCycles
                , ppuState = ppuSt
                , apuState = apuSt
                , cycles = fromIntegral n + cycles bus
                , lastSleepTime = newLastSleepTime
                , cpuInterrupt = interr
                }
    if isNewFrame
        then do
            onNewFrame bus' bus'
            controller' <- liftIO (pollControls bus' $ controller bus')
            cont bus'{controller = controller'} ()
        else
            cont bus' ()

data BusReadOutput = OpenBus | DataBus Byte | Internal Byte

instance MemoryInterface () (Bus r) where
    readByte idx () =
        go >>= \case
            DataBus byte -> modify (\b -> b{dataBus = byte}) $> byte
            OpenBus -> gets dataBus
            Internal byte -> return byte
      where
        go
            | inRange ramRange idx = do
                let mirroredDownAddr = idx .&. 0b11111111111 -- 11 bits
                fmap DataBus . liftIO . readByte mirroredDownAddr =<< gets cpuVram
            | inRange ppuRegisters idx = do
                let mirroredIdx = Addr . fromIntegral $ addrToInt (idx - fst ppuRegisters) `mod` 8
                    onInvalidRead = DataBus <$> gets (ioBus . ppuState)
                case mirroredIdx of
                    0 ->
                        if idx == 0x2000
                            then onInvalidRead
                            else
                                let
                                    addr1 = idx .&. 0b0010000000000111
                                 in
                                    DataBus <$> readByte addr1 ()
                    1 -> onInvalidRead
                    2 -> liftPPU $ do
                        st <- readStatus
                        -- https://www.nesdev.org/wiki/PPU_registers#PPUSTATUS_-_Rendering_events_($2002_read)
                        PPUM.modifyPPUState $ modifyStatusRegister $ clearFlag VBlankStarted
                        oldIoBusState <- withPPUState ioBus
                        let newIoBus = (st .&. 0b11100000) .|. (oldIoBusState .&. 0b11111)
                        PPUM.modifyPPUState $ setIOBus newIoBus
                        return $ DataBus st
                    3 -> onInvalidRead
                    4 -> do
                        res <- liftPPU readOamData
                        modify $ modifyPPUState $ setIOBus res
                        return $ DataBus res
                    5 -> onInvalidRead
                    6 -> onInvalidRead
                    7 -> do
                        res <- liftPPU readData
                        modify $ modifyPPUState $ setIOBus res
                        return $ DataBus res
                    _ -> error "Cannot happen"
            | inRange prgRomRange idx = do
                rom <- gets cartridge
                DataBus <$> readPrgRomAddr (idx - fst prgRomRange) rom readByte
            | idx == 0x4014 = return $ DataBus 0
            | idx == 0x4016 = do
                res <- liftController readButtonStatus
                oldDataBusState <- gets dataBus
                let newDataBus = (oldDataBusState .&. 0b11100000) .|. (res .&. 0b11111)
                return $ DataBus newDataBus
            | idx == 0x4017 = do
                oldDataBusState <- gets dataBus
                return $ DataBus $ oldDataBusState .&. 0b11100000
            | (0x4000, 0x4017) `inRange` idx = do
                res <- liftAPU $ readFromAPU idx
                case res of
                    Nothing -> return OpenBus
                    Just b -> do
                        b' <- do
                            if idx == 0x4015
                                then do
                                    bit5 <- gets $ (`testBit` 5) . dataBus
                                    return $ if bit5 then b `setBit` 5 else b `clearBit` 5
                                else return b
                        return $ Internal b'
            | otherwise = return OpenBus

    writeByte byte idx () = guardWriteBound idx $ do
        modify $ \bus -> bus{dataBus = byte}
        go
      where
        go
            | inRange ramRange idx =
                let
                    addr = idx .&. 0b11111111111
                 in
                    liftIO . writeByte byte addr =<< gets cpuVram
            | inRange ppuRegisters idx = do
                let mirroredIdx = Addr . fromIntegral $ addrToInt (idx - fst ppuRegisters) `mod` 8
                modify $ modifyPPUState $ setIOBus byte
                case mirroredIdx of
                    0 ->
                        if idx == 0x2000
                            then liftPPU $ writeToControlRegister byte
                            else
                                let
                                    addr = idx .&. 0b0010000000000111
                                 in
                                    writeByte byte addr ()
                    1 -> liftPPU $ setMaskRegister byte
                    2 -> return ()
                    3 -> liftPPU $ setOamOffset byte
                    4 -> liftPPU $ writeOamData byte
                    5 -> liftPPU $ setScrollRegister byte
                    6 -> liftPPU $ writeToAddressRegister byte
                    7 -> liftPPU $ writeData byte
                    _ -> error "Cannot happen"
            | inRange prgRomRange idx = liftIO $ putStrLn "Cannot write to catridge"
            | idx == 0x4014 = do
                let high = byteToAddr byte `shiftL` 8
                bytes <- forM [0 .. oamDataSize - 1] $ \i -> do
                    readByte (high + Addr (fromIntegral i)) ()
                liftPPU $ writeListToOam bytes
                cycles_ <- gets Nes.Bus.State.cycles
                -- TODO 1) ticks should be done 256 * 2 (as it's a writting operarion) times
                -- TODO 2) Not sure about about the tick count
                tick (513 + fromEnum (odd cycles_))
            | idx == 0x4016 = liftController $ setStrobe byte
            | (0x4000, 0x4017) `inRange` idx = liftAPU $ writeToAPU idx byte
            | otherwise = pure () -- liftIO $ printf "Ignoring write at %4x\n" $ unAddr idx
    readAddr idx () = do
        low <- readByte idx ()
        high <- readByte (idx + 1) ()
        return $ bytesToAddr low high

    writeAddr addr idx () = do
        let low = unsafeAddrToByte (addr .&. 0xff)
            high = unsafeAddrToByte (addr `shiftR` 8)
        writeByte low idx ()
        writeByte high (idx + 1) ()

{-# INLINE guardWriteBound #-}
guardWriteBound :: (MonadFail m) => Addr -> m () -> m ()
guardWriteBound idx = when (idx < memorySize)

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
