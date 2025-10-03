module PPUSpec (spec) where

import Control.Monad
import qualified Data.ByteString as BS
import Nes.FlagRegister (getFlag, setFlag)
import Nes.Memory (MemoryInterface (readByte, writeByte))
import Nes.PPU.Monad
import Nes.PPU.Pointers
import Nes.PPU.State
import Nes.Rom
import Test.Hspec

spec :: Spec
spec = do
    describe "VRAM Access" $ do
        it "Write to addr" $
            withNewPPU
                noSetup
                ( do
                    writeToAddressRegister 0x23
                    writeToAddressRegister 0x05
                    writeData 0x66
                )
                -- Note: We write using the mirror. 0x0305 is the actual, unmirrored addr
                (\() _ ptrs -> readByte 0x0305 (vram ptrs) `shouldReturn` 0x66)
        it "Read from addr" $
            withNewPPU
                (\st ptrs -> writeByte 0x01 0x0305 (vram ptrs) >> return st)
                ( do
                    writeToAddressRegister 0x23
                    writeToAddressRegister 0x05
                    _ <- readData -- Load data into buffer, increment addr
                    readData -- Returns buffer, increments addr
                )
                ( \res st _ -> do
                    res `shouldBe` 0x01
                    addressRegisterGet (addressRegister st) `shouldBe` 0x2307
                    internalBuffer st `shouldBe` 0x00 -- should have been reset
                )
        it "Read across page" $
            withNewPPU
                ( \st ptrs -> do
                    writeByte 0x01 0x01ff (vram ptrs)
                    writeByte 0x02 0x0200 (vram ptrs)
                    return st
                )
                ( do
                    writeToAddressRegister 0x21
                    writeToAddressRegister 0xff
                    _ <- readData
                    a <- readData
                    b <- readData
                    return (a, b)
                )
                ( \(a, b) _ _ -> do
                    a `shouldBe` 0x01
                    b `shouldBe` 0x02
                )
        it "Read from mirror-ed down addr " $
            withNewPPU
                ( \st ptrs -> do
                    writeByte 0x02 0x0305 (vram ptrs)
                    return st
                )
                ( do
                    writeToAddressRegister 0x63
                    writeToAddressRegister 0x05
                    readData >> readData
                )
                ( \a _ _ -> do
                    a `shouldBe` 0x02
                )

        it "Read and increment addr by 32" $
            withNewPPU
                ( \st ptrs -> do
                    writeByte 0x01 0x01ff (vram ptrs)
                    writeByte 0x02 (0x01ff + 32) (vram ptrs)
                    writeByte 0x03 (0x01ff + 64) (vram ptrs)
                    return $ modifyControlRegister (setFlag VramAddIncrement) st
                )
                ( do
                    writeToAddressRegister 0x21
                    writeToAddressRegister 0xff
                    _ <- readData
                    a <- readData
                    b <- readData
                    c <- readData
                    return (a, b, c)
                )
                ( \(a, b, c) _ _ -> do
                    a `shouldBe` 0x01
                    b `shouldBe` 0x02
                    c `shouldBe` 0x03
                )

    describe "OAM Access" $ do
        it "Write and increment addr" $
            withNewPPU
                noSetup
                ( do
                    setOamOffset 0x10
                    writeOamData 0x01
                    writeOamData 0x02

                    setOamOffset 0x10
                    a <- readOamData
                    setOamOffset 0x11
                    b <- readOamData
                    return (a, b)
                )
                ( \(a, b) _ _ -> do
                    a `shouldBe` 1
                    b `shouldBe` 2
                )
        it "Read and wrap around " $
            withNewPPU
                noSetup
                ( do
                    setOamOffset 0x10
                    let list = (0x01 : replicate 254 0) ++ [0x03]
                    writeListToOam list
                    setOamOffset 0x10
                    a <- readOamData
                    setOamOffset 0xf -- Wrapping around
                    b <- readOamData
                    return (a, b)
                )
                ( \(a, b) _ _ -> do
                    a `shouldBe` 1
                    b `shouldBe` 3
                )

    describe "Reading Status" $ do
        it "Reset VBlank Flag" $
            withNewPPU
                ( \st _ -> do
                    return $ modifyStatusRegister (setFlag VBlankStarted) st
                )
                ( do
                    a <- MkSR <$> readStatus
                    b <- withPPUState statusRegister
                    return (a, b)
                )
                ( \(regA, regB) _ _ -> do
                    getFlag VBlankStarted regA `shouldBe` True
                    -- The flag is reset after the readStatus
                    getFlag VBlankStarted regB `shouldBe` False
                )

        it "Reset latch" $
            withNewPPU
                ( \st ptrs -> do
                    writeByte 0x02 0x0305 (vram ptrs)
                    writeByte 0x03 0x0323 (vram ptrs)
                    return st
                )
                ( do
                    writeToAddressRegister 0x21
                    writeToAddressRegister 0x23
                    writeToAddressRegister 0x23
                    isHighPtr <- withPPUState $ highPtr . addressRegister
                    a <- readData >> readData
                    void readStatus -- Reset latch in address register
                    writeToAddressRegister 0x23
                    writeToAddressRegister 0x05
                    b <- readData >> readData
                    return (isHighPtr, a, b)
                )
                ( \(isHighPtr, a, b) _ _ -> do
                    isHighPtr `shouldBe` False
                    a `shouldBe` 0x03
                    b `shouldBe` 0x02
                )

    describe "Mirroring" $ do
        it "Horizontal" $
            withNewPPU
                noSetup
                ( do
                    -- Write to top right
                    writeToAddressRegister 0x24
                    writeToAddressRegister 0x05
                    writeData 0x01

                    -- Write to bottom left
                    writeToAddressRegister 0x28
                    writeToAddressRegister 0x05
                    writeData 0x02

                    writeToAddressRegister 0x20
                    writeToAddressRegister 0x05
                    _ <- readData -- load in buffer
                    a <- readData -- read from top left
                    writeToAddressRegister 0x2C
                    writeToAddressRegister 0x05
                    _ <- readData
                    b <- readData -- read from bottom right
                    return (a, b)
                )
                ( \(a, b) _ _ -> do
                    a `shouldBe` 0x1
                    b `shouldBe` 0x2
                )

        it "Vertical" $ do
            let st = newPPUState Vertical
            ptrs <- newPPUPointers BS.empty
            withPPU
                st
                ptrs
                ( do
                    -- Write to top left
                    writeToAddressRegister 0x20
                    writeToAddressRegister 0x05
                    writeData 0x01

                    -- Write to bottom right
                    writeToAddressRegister 0x2C
                    writeToAddressRegister 0x05
                    writeData 0x02

                    writeToAddressRegister 0x28
                    writeToAddressRegister 0x05
                    _ <- readData -- load in buffer
                    a <- readData -- read from bottom left
                    writeToAddressRegister 0x24
                    writeToAddressRegister 0x05
                    _ <- readData
                    b <- readData -- read from top right
                    return (a, b)
                )
                ( \(a, b) _ -> do
                    a `shouldBe` 0x1
                    b `shouldBe` 0x2
                )

-- Utils

noSetup :: PPUState -> PPUPointers -> IO PPUState
noSetup = const . return

withNewPPU :: (PPUState -> PPUPointers -> IO PPUState) -> PPU (a, PPUState) a -> (a -> PPUState -> PPUPointers -> IO b) -> IO b
withNewPPU setup op next = do
    let st = newPPUState Horizontal
    ptrs <- newPPUPointers BS.empty
    st' <- setup st ptrs
    (res, st'') <- runPPU st' ptrs op
    next res st'' ptrs

withPPU :: PPUState -> PPUPointers -> PPU (a, PPUState) a -> (a -> PPUState -> IO b) -> IO b
withPPU st ptrs op next = do
    (res, st'') <- runPPU st ptrs op
    next res st''
