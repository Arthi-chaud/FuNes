{-# OPTIONS_GHC -Wno-orphans #-}

module Nes.Memory.Unsafe () where

import Control.Monad.IO.Class
import Data.ByteString
import Foreign
import GHC.ForeignPtr (unsafeWithForeignPtr)
import GHC.Storable
import Nes.Memory

-- | Highly unsafe instance that uses pointers directly.
--
-- No bound checks are done.
instance MemoryInterface (Ptr a) where
    readByte idx ptr = Byte <$> liftIO (readWord8OffPtr (castPtr ptr) (fromIntegral $ unAddr idx))
    readAddr idx ptr = liftIO $ do
        let castedPtr = castPtr ptr
            intIdx = addrToInt idx
        low <- fromIntegral <$> readWord8OffPtr castedPtr intIdx
        high <- fromIntegral <$> readWord8OffPtr castedPtr (intIdx + 1)
        return $ shiftL high 8 .|. low
    writeByte byte idx ptr = liftIO $ writeWord8OffPtr (castPtr ptr) (addrToInt idx) $ unByte byte

    writeAddr addr idx ptr = liftIO $ do
        let intIdx = addrToInt idx
        writeWord8OffPtr (castPtr ptr) intIdx (fromIntegral $ unAddr addr)
        writeWord8OffPtr (castPtr ptr) (intIdx + 1) (fromIntegral $ unAddr $ shiftR addr 8)

instance MemoryInterface (ForeignPtr a) where
    readByte idx fptr = liftIO $ unsafeWithForeignPtr fptr (readByte idx)
    readAddr idx fptr = liftIO $ unsafeWithForeignPtr fptr (readAddr idx)
    writeByte byte idx fptr = liftIO $ unsafeWithForeignPtr fptr (writeByte byte idx)
    writeAddr addr idx fptr = liftIO $ unsafeWithForeignPtr fptr (writeAddr addr idx)
