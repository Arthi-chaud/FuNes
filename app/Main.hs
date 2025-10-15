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

newtype TickRef = MkTickRef {lastSleepTime :: Double}

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
    tickRef <- newIORef . MkTickRef =<< getCPUTimeUs
    bus <- newBus rom (onDrawFrame frame texture renderer) 10000 (tickCallback tickRef)
    void $ runProgram bus (pure ())
    destroyRenderer renderer

tickCallback :: IORef TickRef -> Int -> IO Int
tickCallback ref ticks_ = do
    st <- readIORef ref
    !currentTime <- getCPUTimeUs
    let !totalTickDurationUs = tickDurationUs * fromIntegral ticks_
        !deltaTimeUs = currentTime - lastSleepTime st
        !sleepUs = totalTickDurationUs - deltaTimeUs
    if sleepUs > 100 -- Arbitrary
        then do
            let !intSleepUs = floor sleepUs
                !remainingSleepUs = sleepUs - fromIntegral intSleepUs
                !residualTicks = floor $ remainingSleepUs / tickDurationUs
            threadDelay intSleepUs
            writeIORef ref . MkTickRef =<< getCPUTimeUs
            return residualTicks
        else
            -- If neg, we are late
            return $ if sleepUs < 0 then 0 else ticks_
  where
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
