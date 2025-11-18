{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nes.PPU.Monad (
    -- * Monad
    PPU (..),
    runPPU,

    -- * Ticks
    tick,

    -- * State
    withPointers,
    withPPUState,
    modifyPPUState,

    -- * Vram
    readData,
    writeData,
    mirrorVramAddr,
    incrementVramAddr,

    -- * Registers
    writeToAddressRegister,
    writeToControlRegister,
    setMaskRegister,
    setOamOffset,
    setScrollRegister,

    -- * Status
    readStatus,

    -- * OAM
    readOamData,
    writeOamData,
    writeListToOam,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import qualified Data.ByteString as BS
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import Data.Ix
import Nes.FlagRegister
import Nes.Memory
import Nes.Memory.Unsafe ()
import Nes.PPU.Constants
import Nes.PPU.Pointers
import Nes.PPU.State
import Nes.Rom (Mirroring (..), Rom, chrRom)

newtype PPU r a = MkPPU
    { unPPU ::
        PPUState ->
        PPUPointers ->
        Rom ->
        ( PPUState ->
          PPUPointers ->
          a ->
          IO r
        ) -> -- Continuation
        IO r
    }
    deriving (Functor)

{-# INLINE runPPU #-}
runPPU :: PPUState -> PPUPointers -> Rom -> PPU (a, PPUState) a -> IO (a, PPUState)
runPPU st ptrs rom f = unPPU op st ptrs rom $ \_ _ a -> return a
  where
    op = f >>= \a -> withPPUState (a,)

instance Applicative (PPU r) where
    {-# INLINE pure #-}
    pure a = MkPPU $ \st ptr _ cont -> cont st ptr a

    {-# INLINE liftA2 #-}
    liftA2 f (MkPPU a) (MkPPU b) = MkPPU $ \st ptr rom cont ->
        a st ptr rom $ \st' ptr' aRes ->
            b st' ptr' rom $ \st'' ptr'' bRes ->
                cont st'' ptr'' (f aRes bRes)

instance Monad (PPU r) where
    {-# INLINE (>>=) #-}
    (MkPPU a) >>= next = MkPPU $ \st ptr rom cont ->
        a st ptr rom $ \st' ptr' aRes ->
            unPPU (next aRes) st' ptr' rom cont

instance (MonadIO (PPU r)) where
    {-# INLINE liftIO #-}
    liftIO io = MkPPU $ \st ptr _ cont ->
        io >>= cont st ptr

instance (MonadFail (PPU r)) where
    {-# INLINE fail #-}
    fail s = liftIO $ fail s

tick :: Int -> PPU r Bool
tick cycles_ = do
    setCycles (+ cycles_)
    newCycles <- withPPUState cycles
    if newCycles >= 341
        then do
            hits <- isSpriteZeroHit newCycles
            when hits $
                modifyPPUState $
                    modifyStatusRegister $
                        setFlag SpriteZeroHit
            setCycles (\c -> c - 341)
            modifyPPUState $ \st -> st{scanline = scanline st + 1}
            scanline_ <- withPPUState scanline
            when (scanline_ == 241) $ do
                modifyPPUState $
                    modifyStatusRegister $
                        setFlag VBlankStarted . clearFlag SpriteZeroHit
                shouldStartNmi <- withPPUState $ getFlag GenerateNMI . controlRegister
                modifyPPUState $ \st -> st{nmiInterrupt = shouldStartNmi}
            if scanline_ >= 262
                then do
                    modifyPPUState $ \st -> st{scanline = 0, nmiInterrupt = False}
                    modifyPPUState $
                        modifyStatusRegister $
                            clearFlag SpriteZeroHit . clearFlag VBlankStarted
                    return True
                else return False
        else return False

{-# INLINE setCycles #-}
setCycles :: (Int -> Int) -> PPU r ()
setCycles f = modifyPPUState $ \st -> st{cycles = f (cycles st)}

isSpriteZeroHit :: Int -> PPU r Bool
isSpriteZeroHit cycle_ = do
    scanline_ <- withPPUState scanline
    line <- unAddr . byteToAddr <$> (readByte 0 =<< withPointers oamData)
    col <- byteToInt <$> (readByte 3 =<< withPointers oamData)
    showSprites <- withPPUState $ getFlag ShowSprites . maskRegister
    return $ (line == scanline_) && col <= cycle_ && showSprites

{-# INLINE withPointers #-}
withPointers :: (PPUPointers -> a) -> PPU r a
withPointers f = MkPPU $ \st ptr _ cont ->
    cont st ptr (f ptr)

{-# INLINE withPPUState #-}
withPPUState :: (PPUState -> a) -> PPU r a
withPPUState f = MkPPU $ \st ptr _ cont ->
    cont st ptr (f st)

{-# INLINE modifyPPUState #-}
modifyPPUState :: (PPUState -> PPUState) -> PPU r ()
modifyPPUState f = MkPPU $ \st ptr _ cont ->
    cont (f st) ptr ()

{-# INLINE incrementVramAddr #-}
incrementVramAddr :: PPU r ()
incrementVramAddr = modifyPPUState $ \st ->
    st
        { addressRegister =
            addressRegisterIncrement
                (vramAddrIncrement $ controlRegister st)
                (addressRegister st)
        }

readStatus :: PPU r Byte
readStatus = do
    byte <- unSR <$> withPPUState statusRegister
    modifyPPUState $
        modifyStatusRegister (clearFlag VBlankStarted)
            . modifyAddressRegister addressRegisterResetLatch
            . modifyScrollRegister scrollRegisterResetLatch
    return byte

readOamData :: PPU r Byte
readOamData = do
    oam <- withPointers oamData
    addr <- withPPUState oamOffset
    readByte (byteToAddr addr) oam

writeOamData :: Byte -> PPU r ()
writeOamData byte = do
    oam <- withPointers oamData
    addr <- withPPUState oamOffset
    writeByte byte (byteToAddr addr) oam
    setOamOffset (addr + 1)

{-# INLINE writeListToOam #-}
-- AKA DMA
writeListToOam :: [Byte] -> PPU r ()
writeListToOam = foldlM (\_ item -> writeOamData item) ()

{-# INLINE writeToAddressRegister #-}
writeToAddressRegister :: Byte -> PPU r ()
writeToAddressRegister byte =
    modifyPPUState $
        \st -> st{addressRegister = addressRegisterUpdate byte (addressRegister st)}

writeToControlRegister :: Byte -> PPU r ()
writeToControlRegister byte = do
    oldNmi <- withPPUState $ getFlag GenerateNMI . controlRegister
    let newCR = MkCR byte
        newNmi = getFlag GenerateNMI newCR
    modifyPPUState $ \st -> st{controlRegister = newCR}
    isInVBlank <- withPPUState $ getFlag VBlankStarted . statusRegister
    when (not oldNmi && newNmi && isInVBlank) $
        modifyPPUState $
            \st -> st{nmiInterrupt = True}

{-# INLINE setMaskRegister #-}
setMaskRegister :: Byte -> PPU r ()
setMaskRegister byte = modifyPPUState $ \st -> st{maskRegister = MkMR byte}

{-# INLINE setOamOffset #-}
setOamOffset :: Byte -> PPU r ()
setOamOffset byte = modifyPPUState $ \st -> st{oamOffset = byte}

{-# INLINE setScrollRegister #-}
setScrollRegister :: Byte -> PPU r ()
setScrollRegister byte =
    modifyPPUState $
        modifyScrollRegister $
            scrollRegisterWrite byte

{-# INLINE withCartridge #-}
withCartridge :: (Rom -> a) -> PPU r a
withCartridge f = MkPPU $ \st ptrs rom cont -> cont st ptrs (f rom)

readData :: PPU r Byte
readData = do
    addr <- withPPUState $ addressRegisterGet . addressRegister
    res <- go addr
    incrementVramAddr
    return res
  where
    go addr
        | inRange chrRomRange addr = do
            res <- withPPUState internalBuffer
            value <- Byte <$> (withCartridge chrRom <&> (`BS.index` addrToInt addr))
            modifyPPUState $ \st -> st{internalBuffer = value}
            return res
        | inRange vramRange addr = do
            res <- withPPUState internalBuffer
            mirr <- withPPUState mirroring
            value <- readByte (mirrorVramAddr mirr addr) =<< withPointers vram
            modifyPPUState $ \st -> st{internalBuffer = value}
            return res
        | inRange unusedAddrRange addr = do
            liftIO $ putStrLn "Address range should not be accessed"
            return 0
        | inRange paletteTableRange addr = do
            plt <- withPointers paletteTable
            -- https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch6.1/src/ppu/mod.rs#L169
            let addr1 =
                    if addr `elem` paletteIndexes
                        then addr - 0x10
                        else addr
            liftIO $ readByte (addr1 - 0x3f00) plt
        | otherwise = do
            liftIO $ putStrLn "Unexpected access to mirrored space"
            return 0

writeData :: Byte -> PPU r ()
writeData byte = do
    addr <- withPPUState $ addressRegisterGet . addressRegister
    incrementVramAddr
    go addr
  where
    go addr
        | inRange chrRomRange addr = liftIO $ putStrLn "Invalid write to CHR Rom"
        | inRange vramRange addr = do
            mirr <- withPPUState mirroring
            writeByte byte (mirrorVramAddr mirr addr) =<< withPointers vram
        | inRange unusedAddrRange addr = liftIO $ putStrLn "Invalid write in address space"
        | inRange paletteTableRange addr = do
            plt <- withPointers paletteTable
            let addr1 =
                    if addr `elem` paletteIndexes
                        then addr - 0x10
                        else addr
            liftIO $ writeByte byte (addr1 - 0x3f00) plt
        | otherwise = liftIO $ putStrLn "Unexpected access to mirrored space"

{-# INLINE mirrorVramAddr #-}
mirrorVramAddr :: Mirroring -> Addr -> Addr
mirrorVramAddr mirr addr = case (mirr, nameTable) of
    (Vertical, 2) -> vramIndex - 0x800
    (Vertical, 3) -> vramIndex - 0x800
    (Horizontal, 2) -> vramIndex - 0x400
    (Horizontal, 1) -> vramIndex - 0x400
    (Horizontal, 3) -> vramIndex - 0x800
    _ -> vramIndex
  where
    mirroredVram = addr .&. 0b10111111111111
    vramIndex = mirroredVram - 0x2000
    nameTable = unAddr vramIndex `div` 0x400
