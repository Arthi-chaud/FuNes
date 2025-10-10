{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Array.Base (MArray (unsafeRead, unsafeWrite))
import Data.Array.IO.Internals (IOUArray)
import Data.Array.MArray
import Data.ByteString.Internal (create)
import Data.Functor (($>))
import Data.Word (Word8)
import GHC.Storable (writeWord8OffPtr)
import Nes.Bus
import Nes.Bus.Monad ()
import Nes.CPU.Interpreter
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory
import Nes.Rom
import SDL
import SDL.Internal.Types
import SDL.Raw (Color (Color), renderSetScale)
import System.Exit (exitSuccess)
import System.Random
import Text.Printf (printf)

-- Source: https://bugzmanov.github.io/nes_ebook/chapter_3_4.html

frameSize :: Int
frameSize = 32 * 3 * 32

programOffset :: Addr
programOffset = 0x600

main :: IO ()
main = do
    initializeAll
    let windowConfig =
            defaultWindow
                { windowInitialSize = V2 320 320
                , windowPosition = Centered
                }
    window <- createWindow "Fake Snake Game" windowConfig
    renderer@(Renderer rendererPtr) <- createRenderer window (-1) defaultRenderer
    _ <- setHintWithPriority NormalPriority HintRenderVSync DisableVSync
    _ <- renderSetScale rendererPtr 32 32
    texture <- createTexture renderer RGB24 TextureAccessTarget (V2 32 32)
    frame <- newArray @IOUArray (0, frameSize) (0 :: Word8)
    let cpuState = newCPUState{programCounter = programOffset}
    bus <- newBus unsafeEmptyRom pure (const $ pure ())
    loadProgramToMemory gameCode bus
    _ <- runProgram' cpuState bus (callback frame texture renderer)
    destroyRenderer renderer

callback :: IOUArray Int Word8 -> Texture -> Renderer -> CPU r ()
callback frame texture renderer = do
    pc <- getPC
    opCode <- unsafeWithBus $ readByte pc ()
    b1 <- unsafeWithBus $ readByte (pc + 1) ()
    b2 <- unsafeWithBus $ readByte (pc + 2) ()
    (a, x, y, stack, statusFlags) <- withCPUState $ \st -> (registerA st, registerX st, registerY st, registerS st, status st)
    liftIO $ printf "OP: 0x%02x (0x%02x 0x%02x), A: %d, X: %d, Y: %d, PC: 0x%02x, Stack: 0x%02x, Flag: 0b%08b\n" (unByte opCode) (unByte b1) (unByte b2) (unByte a) (unByte x) (unByte y) (unAddr pc) (unByte stack) (unByte $ unSR statusFlags)
    handleEvents
    randomByte <- Byte <$> getStdRandom (randomR (1, 16))
    unsafeWithBus $ writeByte randomByte 0xfe ()
    frameHasChanged <- readScreenState frame
    when frameHasChanged $ liftIO $ do
        bs <- create frameSize $ \ptr -> void $ foldlMArrayM' (\idx e -> writeWord8OffPtr ptr idx e $> idx + 1) 0 frame
        SDL.updateTexture texture Nothing bs (32 * 3)
        copy renderer texture Nothing Nothing
        present renderer

readScreenState :: IOUArray Int Word8 -> CPU r Bool
readScreenState frame = updatePixel False 0x200 0
  where
    updatePixel :: Bool -> Addr -> Int -> CPU r Bool
    updatePixel acc addr _ | addr >= 0x600 = return acc
    updatePixel acc memAddr frameByte = do
        colorByte <- unsafeWithBus $ readByte memAddr ()
        let Color b1 b2 b3 _ = toColor $ unByte colorByte
        currentB1 <- liftIO $ unsafeRead frame frameByte
        currentB2 <- liftIO $ unsafeRead frame (frameByte + 1)
        currentB3 <- liftIO $ unsafeRead frame (frameByte + 2)
        let hasChanged = currentB1 /= b1 || currentB2 /= b2 || currentB3 /= b3
        when hasChanged $ liftIO $ do
            unsafeWrite frame frameByte b1
            unsafeWrite frame (frameByte + 1) b2
            unsafeWrite frame (frameByte + 2) b3
        updatePixel (acc || hasChanged) (memAddr + 1) (frameByte + 3)
toColor :: Word8 -> Color
toColor = \case
    0 -> Color 0 0 0 1
    1 -> Color maxBound maxBound maxBound 1
    2 -> Color 0x80 0x80 0x80 1
    9 -> Color 0x80 0x80 0x80 1
    3 -> Color 0xff 0 0 1
    10 -> Color 0xff 0 0 1
    4 -> Color 0 0xff 0 1
    11 -> Color 0 0xff 0 1
    5 -> Color 0 0 0xff 1
    12 -> Color 0 0 0xff 1
    6 -> Color 0xff 0 0xff 1
    13 -> Color 0xff 0 0xff 1
    7 -> Color 0xff 0xff 0 1
    14 -> Color 0xff 0xff 0 1
    _ -> Color 0 0xff 0xff 1

handleEvents :: CPU r ()
handleEvents = do
    events <- pollEvents
    forM_ events (go . eventPayload)
  where
    exit = liftIO exitSuccess
    go = \case
        QuitEvent -> exit
        KeyboardEvent (KeyboardEventData _ _ _ sym) -> case SDL.keysymScancode sym of
            ScancodeQ -> exit
            ScancodeEscape -> exit
            ScancodeUp -> unsafeWithBus $ writeByte 0x77 0xff ()
            ScancodeDown -> unsafeWithBus $ writeByte 0x73 0xff ()
            ScancodeLeft -> unsafeWithBus $ writeByte 0x61 0xff ()
            ScancodeRight -> unsafeWithBus $ writeByte 0x64 0xff ()
            _ -> pure ()
        _ -> pure ()

gameCode :: [Word8]
gameCode =
    [ 0x20
    , 0x06
    , 0x06
    , 0x20
    , 0x38
    , 0x06
    , 0x20
    , 0x0d
    , 0x06
    , 0x20
    , 0x2a
    , 0x06
    , 0x60
    , 0xa9
    , 0x02
    , 0x85
    , 0x02
    , 0xa9
    , 0x04
    , 0x85
    , 0x03
    , 0xa9
    , 0x11
    , 0x85
    , 0x10
    , 0xa9
    , 0x10
    , 0x85
    , 0x12
    , 0xa9
    , 0x0f
    , 0x85
    , 0x14
    , 0xa9
    , 0x04
    , 0x85
    , 0x11
    , 0x85
    , 0x13
    , 0x85
    , 0x15
    , 0x60
    , 0xa5
    , 0xfe
    , 0x85
    , 0x00
    , 0xa5
    , 0xfe
    , 0x29
    , 0x03
    , 0x18
    , 0x69
    , 0x02
    , 0x85
    , 0x01
    , 0x60
    , 0x20
    , 0x4d
    , 0x06
    , 0x20
    , 0x8d
    , 0x06
    , 0x20
    , 0xc3
    , 0x06
    , 0x20
    , 0x19
    , 0x07
    , 0x20
    , 0x20
    , 0x07
    , 0x20
    , 0x2d
    , 0x07
    , 0x4c
    , 0x38
    , 0x06
    , 0xa5
    , 0xff
    , 0xc9
    , 0x77
    , 0xf0
    , 0x0d
    , 0xc9
    , 0x64
    , 0xf0
    , 0x14
    , 0xc9
    , 0x73
    , 0xf0
    , 0x1b
    , 0xc9
    , 0x61
    , 0xf0
    , 0x22
    , 0x60
    , 0xa9
    , 0x04
    , 0x24
    , 0x02
    , 0xd0
    , 0x26
    , 0xa9
    , 0x01
    , 0x85
    , 0x02
    , 0x60
    , 0xa9
    , 0x08
    , 0x24
    , 0x02
    , 0xd0
    , 0x1b
    , 0xa9
    , 0x02
    , 0x85
    , 0x02
    , 0x60
    , 0xa9
    , 0x01
    , 0x24
    , 0x02
    , 0xd0
    , 0x10
    , 0xa9
    , 0x04
    , 0x85
    , 0x02
    , 0x60
    , 0xa9
    , 0x02
    , 0x24
    , 0x02
    , 0xd0
    , 0x05
    , 0xa9
    , 0x08
    , 0x85
    , 0x02
    , 0x60
    , 0x60
    , 0x20
    , 0x94
    , 0x06
    , 0x20
    , 0xa8
    , 0x06
    , 0x60
    , 0xa5
    , 0x00
    , 0xc5
    , 0x10
    , 0xd0
    , 0x0d
    , 0xa5
    , 0x01
    , 0xc5
    , 0x11
    , 0xd0
    , 0x07
    , 0xe6
    , 0x03
    , 0xe6
    , 0x03
    , 0x20
    , 0x2a
    , 0x06
    , 0x60
    , 0xa2
    , 0x02
    , 0xb5
    , 0x10
    , 0xc5
    , 0x10
    , 0xd0
    , 0x06
    , 0xb5
    , 0x11
    , 0xc5
    , 0x11
    , 0xf0
    , 0x09
    , 0xe8
    , 0xe8
    , 0xe4
    , 0x03
    , 0xf0
    , 0x06
    , 0x4c
    , 0xaa
    , 0x06
    , 0x4c
    , 0x35
    , 0x07
    , 0x60
    , 0xa6
    , 0x03
    , 0xca
    , 0x8a
    , 0xb5
    , 0x10
    , 0x95
    , 0x12
    , 0xca
    , 0x10
    , 0xf9
    , 0xa5
    , 0x02
    , 0x4a
    , 0xb0
    , 0x09
    , 0x4a
    , 0xb0
    , 0x19
    , 0x4a
    , 0xb0
    , 0x1f
    , 0x4a
    , 0xb0
    , 0x2f
    , 0xa5
    , 0x10
    , 0x38
    , 0xe9
    , 0x20
    , 0x85
    , 0x10
    , 0x90
    , 0x01
    , 0x60
    , 0xc6
    , 0x11
    , 0xa9
    , 0x01
    , 0xc5
    , 0x11
    , 0xf0
    , 0x28
    , 0x60
    , 0xe6
    , 0x10
    , 0xa9
    , 0x1f
    , 0x24
    , 0x10
    , 0xf0
    , 0x1f
    , 0x60
    , 0xa5
    , 0x10
    , 0x18
    , 0x69
    , 0x20
    , 0x85
    , 0x10
    , 0xb0
    , 0x01
    , 0x60
    , 0xe6
    , 0x11
    , 0xa9
    , 0x06
    , 0xc5
    , 0x11
    , 0xf0
    , 0x0c
    , 0x60
    , 0xc6
    , 0x10
    , 0xa5
    , 0x10
    , 0x29
    , 0x1f
    , 0xc9
    , 0x1f
    , 0xf0
    , 0x01
    , 0x60
    , 0x4c
    , 0x35
    , 0x07
    , 0xa0
    , 0x00
    , 0xa5
    , 0xfe
    , 0x91
    , 0x00
    , 0x60
    , 0xa6
    , 0x03
    , 0xa9
    , 0x00
    , 0x81
    , 0x10
    , 0xa2
    , 0x00
    , 0xa9
    , 0x01
    , 0x81
    , 0x10
    , 0x60
    , 0xa2
    , 0x00
    , 0xea
    , 0xea
    , 0xca
    , 0xd0
    , 0xfb
    , 0x60
    ]

loadProgramToMemory :: [Word8] -> Bus -> IO ()
loadProgramToMemory program bus = do
    forM_ (zip program [(unAddr programOffset) ..]) $
        \(byte, idx) -> writeByte (Byte byte) (Addr idx) (cpuVram bus)
