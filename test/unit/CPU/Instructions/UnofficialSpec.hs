module CPU.Instructions.UnofficialSpec (spec) where

import Internal
import Nes.Bus.State (BusState (cpuVram))
import Nes.CPU.State
import Nes.Memory
import Test.Hspec

spec :: Spec
spec = do
    -- Based on AccuracyCoin
    describe "SHA" $ do
        -- Test A
        describe "Absolute Y" $ do
            it "Crosses page, goes unstable" $ do
                let program = [0x9f, 0x80, 0x1e]
                let st =
                        newCPUState
                            { _registerA = 0x1f
                            , _registerX = 0xff
                            , _registerY = 0x80
                            }
                    setup _ = return ()
                withStateAndMemorySetup program st setup $ \_ bus ->
                    readByte 0x700 (cpuVram bus) `shouldReturn` 0x1f

            it "Does not cross page" $ do
                let program = [0x9f, 0x80, 0x1e]
                let st =
                        newCPUState
                            { _registerA = 0x1f
                            , _registerX = 0xff
                            , _registerY = 0x01
                            }
                    setup _ = return ()
                withStateAndMemorySetup program st setup $ \_ bus ->
                    readByte 0x681 (cpuVram bus) `shouldReturn` 0x1f

        -- Test E
        describe "Indirect Y" $ do
            it "Crosses page, goes unstable" $ do
                let program = [0x93, 0x50]
                let st =
                        newCPUState
                            { _registerA = 0x1f
                            , _registerX = 0xff
                            , _registerY = 0x80
                            }
                    setup = writeAddr 0x1e80 0x50 . cpuVram
                withStateAndMemorySetup program st setup $ \_ bus ->
                    readByte 0x700 (cpuVram bus) `shouldReturn` 0x1f

    describe "SHX" $ do
        -- Test B
        it "Crosses page, goes unstable" $ do
            let program = [0x9e, 0x80, 0x1e]
            let st =
                    newCPUState
                        { _registerA = 0x00
                        , _registerX = 0x5
                        , _registerY = 0x80
                        }
                setup _ = return ()
            withStateAndMemorySetup program st setup $ \_ bus ->
                readByte 0x500 (cpuVram bus) `shouldReturn` 0x5

        it "Does not cross page" $ do
            let program = [0x9e, 0x80, 0x1e]
            let st =
                    newCPUState
                        { _registerA = 0x00
                        , _registerX = 0x5
                        , _registerY = 0x01
                        }
                setup _ = return ()
            withStateAndMemorySetup program st setup $ \_ bus ->
                readByte 0x681 (cpuVram bus) `shouldReturn` 0x5

    describe "SHY" $ do
        -- Test C
        it "Crosses page, goes unstable" $ do
            let program = [0x9c, 0x80, 0x1e]
            let st =
                    newCPUState
                        { _registerA = 0x00
                        , _registerX = 0x80
                        , _registerY = 0x5
                        }
                setup _ = return ()
            withStateAndMemorySetup program st setup $ \_ bus ->
                readByte 0x500 (cpuVram bus) `shouldReturn` 0x5

        it "Does not cross page" $ do
            let program = [0x9c, 0x80, 0x1e]
            let st =
                    newCPUState
                        { _registerA = 0x00
                        , _registerX = 0x01
                        , _registerY = 0x5
                        }
                setup _ = return ()
            withStateAndMemorySetup program st setup $ \_ bus ->
                readByte 0x681 (cpuVram bus) `shouldReturn` 0x5

    describe "SHS" $ do
        -- Test D
        it "Crosses page, goes unstable" $ do
            let program = [0x9b, 0x80, 0x1e, 0xba]
            let st =
                    newCPUState
                        { _registerA = 0x1f
                        , _registerX = 0xff
                        , _registerY = 0x80
                        }
                setup _ = return ()
            withStateAndMemorySetup program st setup $ \_ bus ->
                readByte 0x700 (cpuVram bus) `shouldReturn` 0x1f

        it "Does not cross page" $ do
            let program = [0x9b, 0x80, 0x1e, 0xba]
            let st =
                    newCPUState
                        { _registerA = 0x1f
                        , _registerX = 0xff
                        , _registerY = 0x01
                        }
                setup _ = return ()
            withStateAndMemorySetup program st setup $ \_ bus ->
                readByte 0x681 (cpuVram bus) `shouldReturn` 0x1f
