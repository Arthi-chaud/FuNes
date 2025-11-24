module Nes.PPU.Monad (
    -- * Monad
    PPU (..),
    runPPU,

    -- * Ticks
    tick,

    -- * State
    withPointers,

    -- * Vram
    readData,
    writeData,
    mirrorVramAddr,
    incrementVramAddr,

    -- * Registers
    writeToAddressRegister,
    writeToControlRegister,
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
import Nes.Internal.MonadState
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
    op = f >>= \a -> gets (a,)

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

instance MonadState PPUState (PPU r) where
    {-# INLINE get #-}
    get = MkPPU $ \st ptr _ cont -> cont st ptr st
    {-# INLINE set #-}
    set st' = MkPPU $ \_ ptr _ cont -> cont st' ptr ()

tick :: Int -> PPU r Bool
tick cycles_ = do
    cycles += cycles_
    newCycles <- use cycles
    if newCycles >= 341
        then do
            hits <- isSpriteZeroHit newCycles
            when hits $ statusRegister %= setFlag SpriteZeroHit
            cycles += (-341)
            scanline += 1
            scanline_ <- use scanline
            when (scanline_ == 241) $ do
                statusRegister %= (setFlag VBlankStarted . clearFlag SpriteZeroHit)
                shouldStartNmi <- uses controlRegister $ getFlag GenerateNMI
                nmiInterrupt .= shouldStartNmi
            if scanline_ >= 262
                then do
                    scanline .= 0
                    nmiInterrupt .= False
                    statusRegister %= (clearFlag SpriteZeroHit . clearFlag VBlankStarted)
                    return True
                else return False
        else return False

isSpriteZeroHit :: Int -> PPU r Bool
isSpriteZeroHit cycle_ = do
    scanline_ <- use scanline
    line <- unAddr . byteToAddr <$> (readByte 0 =<< withPointers oamData)
    col <- byteToInt <$> (readByte 3 =<< withPointers oamData)
    showSprites <- uses maskRegister $ getFlag ShowSprites
    return $ (line == scanline_) && col <= cycle_ && showSprites

{-# INLINE withPointers #-}
withPointers :: (PPUPointers -> a) -> PPU r a
withPointers f = MkPPU $ \st ptr _ cont ->
    cont st ptr (f ptr)

{-# INLINE incrementVramAddr #-}
incrementVramAddr :: PPU r ()
incrementVramAddr = do
    incr <- uses controlRegister vramAddrIncrement
    addressRegister %= addressRegisterIncrement incr

readStatus :: PPU r Byte
readStatus = do
    byte <- uses statusRegister unSR
    statusRegister %= clearFlag VBlankStarted
    addressRegister %= addressRegisterResetLatch
    scrollRegister %= scrollRegisterResetLatch
    return byte

readOamData :: PPU r Byte
readOamData = do
    oam <- withPointers oamData
    addr <- use oamOffset
    readByte (byteToAddr addr) oam

writeOamData :: Byte -> PPU r ()
writeOamData byte = do
    oam <- withPointers oamData
    addr <- use oamOffset
    writeByte byte (byteToAddr addr) oam
    oamOffset .= (addr + 1)

{-# INLINE writeListToOam #-}
writeListToOam :: [Byte] -> PPU r ()
writeListToOam = foldlM (\_ item -> writeOamData item) ()

{-# INLINE writeToAddressRegister #-}
writeToAddressRegister :: Byte -> PPU r ()
writeToAddressRegister byte = addressRegister %= addressRegisterUpdate byte

writeToControlRegister :: Byte -> PPU r ()
writeToControlRegister byte = do
    oldNmi <- uses controlRegister $ getFlag GenerateNMI
    let newCR = MkCR byte
        newNmi = getFlag GenerateNMI newCR
    controlRegister .= newCR
    isInVBlank <- uses statusRegister $ getFlag VBlankStarted
    when (not oldNmi && newNmi && isInVBlank) $
        nmiInterrupt .= True

{-# INLINE setScrollRegister #-}
setScrollRegister :: Byte -> PPU r ()
setScrollRegister byte = scrollRegister %= scrollRegisterWrite byte

{-# INLINE withCartridge #-}
withCartridge :: (Rom -> a) -> PPU r a
withCartridge f = MkPPU $ \st ptrs rom cont -> cont st ptrs (f rom)

readData :: PPU r Byte
readData = do
    addr <- uses addressRegister addressRegisterGet
    res <- go addr
    incrementVramAddr
    return res
  where
    go addr
        | inRange chrRomRange addr = do
            res <- use internalBuffer
            value <- Byte <$> (withCartridge chrRom <&> (`BS.index` addrToInt addr))
            internalBuffer .= value
            return res
        | inRange vramRange addr = do
            res <- use internalBuffer
            mirr <- use mirroring
            value <- readByte (mirrorVramAddr mirr addr) =<< withPointers vram
            internalBuffer .= value
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
    addr <- uses addressRegister addressRegisterGet
    incrementVramAddr
    go addr
  where
    go addr
        | inRange chrRomRange addr = liftIO $ putStrLn "Invalid write to CHR Rom"
        | inRange vramRange addr = do
            mirr <- use mirroring
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

-- TODO It is a memory interface
