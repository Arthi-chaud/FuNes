module Nes.Internal (callocForeignPtr) where

import Foreign
import Nes.Memory (MemoryPointer)

callocForeignPtr :: Int -> IO MemoryPointer
callocForeignPtr size = do
    ptr <- callocBytes size
    newForeignPtr finalizerFree ptr
