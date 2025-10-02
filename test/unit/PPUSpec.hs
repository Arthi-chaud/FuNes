module PPUSpec (spec) where

import qualified Data.ByteString as BS
import Nes.FlagRegister (setFlag)
import Nes.Memory (MemoryInterface (readByte, writeByte))
import Nes.PPU.Monad
import Nes.PPU.Pointers
import Nes.PPU.State
import Nes.Rom
import Test.Hspec

spec :: Spec
spec = do
    describe "VRAM Access" $ do
        it "should write to addr" $
            withNewPPU
                noSetup
                ( do
                    writeToAddressRegister 0x23
                    writeToAddressRegister 0x05
                    writeData 0x66
                )
                -- Note: We write using the mirror. 0x0305 is the actual, unmirrored addr
                (\() _ ptrs -> readByte 0x0305 (vram ptrs) `shouldReturn` 0x66)
        it "should read from addr" $
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
        it "should read across page" $
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

        it "should read and increment addr by 32" $
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
noSetup :: PPUState -> PPUPointers -> IO PPUState
noSetup = const . return

withNewPPU :: (PPUState -> PPUPointers -> IO PPUState) -> PPU (a, PPUState) a -> (a -> PPUState -> PPUPointers -> IO b) -> IO b
withNewPPU setup op next = do
    let st = newPPUState Horizontal
    ptrs <- newPPUPointers BS.empty
    st' <- setup st ptrs
    (res, st'') <- runPPU st' ptrs op
    next res st'' ptrs
