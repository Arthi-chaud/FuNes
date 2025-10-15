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
import Nes.Render.Monad (runRender, toSDL2ByteString)
import qualified Nes.Render.Monad as R
import Nes.Rom
import SDL
import SDL.Internal.Types
import qualified SDL.Raw as Raw
import System.CPUTime (getCPUTime)
import System.Environment

data TickRef = MkTickRef {tickRemainder :: {-# UNPACK #-} !Int, lastSleepTime :: {-# UNPACK #-} !Double}

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
    frame <- newFrameState
    tickRef <- newIORef . MkTickRef 0 =<< getCPUTimeUs
    bus <- newBus rom (onDrawFrame frame texture renderer) (tickCallback tickRef)
    void $ runProgram bus (pure ())
    destroyRenderer renderer

tickCallback :: IORef TickRef -> Int -> IO ()
tickCallback ref ticks_ = do
    MkTickRef remainingTicks lastSleep <- readIORef ref
    let !totalTicks = remainingTicks + ticks_
    if totalTicks < 1000
        then
            writeIORef ref $! MkTickRef totalTicks lastSleep
        else do
            !currentTime <- getCPUTimeUs
            let !totalTickDurationUs = tickDurationUs * fromIntegral totalTicks
                !deltaTimeUs = currentTime - lastSleep
                !sleepUs = deltaTimeUs - totalTickDurationUs
            if sleepUs > 100 -- Arbitrary
                then do
                    let !intSleepUs = floor sleepUs
                        !remainingSleepUs = sleepUs - fromIntegral intSleepUs
                        !residualTicks = floor $ remainingSleepUs / tickDurationUs
                    writeIORef ref $! MkTickRef residualTicks currentTime
                    threadDelay intSleepUs
                else
                    writeIORef ref $! MkTickRef totalTicks lastSleep
  where
    -- Duration of a tick, in microsecond
    tickDurationUs = (1000000 / cpuFrequency) :: Double
    -- Frequency in Hz
    cpuFrequency = 1.789773 * 1000000

onDrawFrame :: FrameState -> Texture -> Renderer -> Bus -> IO Bus
onDrawFrame frame texture renderer bus = do
    bs <- runRender (render bus R.>> toSDL2ByteString) frame
    updateTexture texture Nothing bs (256 * 3)
    copy renderer texture Nothing Nothing
    present renderer
    snd <$> runBusM bus handleEvents

{-# INLINE getCPUTimeUs #-}
getCPUTimeUs :: IO Double
getCPUTimeUs = (/ 1000000) . fromIntegral <$> getCPUTime
