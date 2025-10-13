module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Events
import Nes.Bus
import Nes.Bus.Monad (runBusM)
import Nes.CPU.Interpreter
import Nes.Render
import Nes.Render.Frame
import Nes.Rom
import SDL
import SDL.Internal.Types
import qualified SDL.Raw as Raw
import System.Environment

main :: IO ()
main = do
    romPath <- do
        args <- getArgs
        case args of
            (path : _) -> return path
            _ -> fail "Expected one argument: Path to the ROM"
    rom <- do
        res <- fromFile romPath
        either fail return res
    tickRef <- newIORef (0 :: Int)
    initializeAll
    let windowConfig =
            defaultWindow
                { windowInitialSize =
                    V2
                        (256 * 3)
                        (240 * 3)
                , windowPosition = Centered
                }
    window <- createWindow "FuNes" windowConfig
    renderer@(Renderer rendererPtr) <-
        createRenderer
            window
            (-1)
            defaultRenderer
    _ <- setHintWithPriority NormalPriority HintRenderVSync DisableVSync
    _ <- Raw.renderSetScale rendererPtr 3 3
    texture <- createTexture renderer RGB24 TextureAccessTarget (V2 256 240)
    frame <- newFrame
    bus <- newBus rom (onDrawFrame frame texture renderer) (tickCallback tickRef)
    void $ runProgram bus (pure ())
    destroyRenderer renderer

tickCallback :: IORef Int -> Int -> IO ()
tickCallback ref ticks_ = do
    old <- readIORef ref
    let res = old + ticks_
        microsecondsToSleep = res `div` 179
    writeIORef ref (res `mod` 179)
    when (microsecondsToSleep > 0) $ threadDelay microsecondsToSleep

onDrawFrame :: Frame -> Texture -> Renderer -> Bus -> IO Bus
onDrawFrame frame texture renderer bus = do
    render frame bus
    updateTexture texture Nothing (frameToByteString frame) (256 * 3)
    copy renderer texture Nothing Nothing
    present renderer
    snd <$> runBusM bus handleEvents
