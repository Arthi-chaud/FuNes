module Nes.Internal (callocForeignPtr) where

import Control.Monad
import Foreign
import GHC.ForeignPtr
import GHC.Storable
import Nes.Memory (MemoryPointer)

callocForeignPtr :: Int -> IO MemoryPointer
callocForeignPtr size = do
    fptr <- mallocForeignPtrBytes size
    unsafeWithForeignPtr fptr $
        \ptr -> forM_ [0 .. size] $ \idx -> writeWord8OffPtr ptr idx 0
    return $ castForeignPtr fptr
