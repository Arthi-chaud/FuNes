{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Control.Monad
import Data.IORef
import qualified Data.Vector.Storable.Mutable as V
import Events
import Nes.APU.State.Filter.Constants
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
import System.Environment

vectorSize :: Int
vectorSize = 4 * sampleCount

sampleCount :: Int
sampleCount = 1024

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
    vectorCursor <- newIORef 0
    sampleVector <- V.new vectorSize
    initializeAll

    let windowConfig =
            defaultWindow
                { windowInitialSize =
                    V2
                        (256 * 3)
                        (240 * 3)
                , windowPosition = Centered
                }
    (device, _) <-
        openAudioDevice
            OpenDeviceSpec
                { SDL.openDeviceFreq = Mandate $ floor defaultOutputRate
                , SDL.openDeviceFormat = Mandate FloatingLEAudio
                , SDL.openDeviceChannels = Mandate Mono
                , SDL.openDeviceSamples = fromIntegral sampleCount
                , SDL.openDeviceCallback = audioCallback sampleVector vectorCursor
                , SDL.openDeviceUsage = ForPlayback
                , SDL.openDeviceName = Nothing
                }
    window <- createWindow "FuNes" windowConfig
    renderer@(Renderer rendererPtr) <-
        createRenderer
            window
            (-1)
            defaultRenderer
    -- _ <- setHintWithPriority NormalPriority HintRenderVSync DisableVSync
    _ <- Raw.renderSetScale rendererPtr 3 3
    texture <- createTexture renderer RGB24 TextureAccessTarget (V2 256 240)
    setAudioDevicePlaybackState device Play
    frame <- newFrameState
    bus <-
        newBus
            rom
            (onDrawFrame frame texture renderer)
            (sampleCallback sampleVector vectorCursor)
            tickCallback
    void $ runProgram bus (pure ())
    closeAudioDevice device
    destroyRenderer renderer

tickCallback :: Double -> Int -> IO (Double, Int)
tickCallback lastSleepTime_ ticks_ = return (lastSleepTime_, ticks_)

sampleCallback :: V.IOVector Float -> IORef Int -> Float -> IO ()
sampleCallback vec cursorRef sample = do
    cursor <- readIORef cursorRef
    when (cursor < vectorSize) $ do
        V.write vec cursor sample
        writeIORef cursorRef (cursor + 1)

--   !currentTime <- getCPUTimeUs
--   let !totalTickDurationUs = tickDurationUs * fromIntegral ticks_
--       !deltaTimeUs = currentTime - lastSleepTime
--       !sleepUs = totalTickDurationUs - deltaTimeUs
--   if ticks_ < 100
--       then return (lastSleepTime, ticks_)
--       else
--           if sleepUs > 500
--               then do
--                   let !intSleepUs = floor sleepUs
--                       !remainingSleepUs = sleepUs - fromIntegral intSleepUs
--                       !residualTicks = floor $ remainingSleepUs / tickDurationUs
--                   -- threadDelay intSleepUs
--                   return (currentTime, residualTicks)
--               else
--                   if sleepUs < 0
--                       then do
--                           return (currentTime, 0)
--                       else do
--                           return (lastSleepTime, ticks_)
-- where
--   tickDurationUs = (1000000 / cpuFrequency) :: Double
--   -- Frequency in Hz
--   cpuFrequency = 1.789773 * 1000000

audioCallback :: V.IOVector Float -> IORef Int -> AudioFormat sampleType -> V.IOVector sampleType -> IO ()
audioCallback samples cursorRef fmt buffer = case fmt of
    FloatingLEAudio -> do
        cursor <- readIORef cursorRef
        let bufferLen = V.length buffer
            nToCopy = min bufferLen cursor
        when (cursor < bufferLen) $ do
            V.set buffer 0
        V.copy (V.slice 0 nToCopy buffer) (V.slice 0 nToCopy samples)
        -- If more samples are ready
        if cursor > bufferLen
            then do
                let toShift = cursor - bufferLen
                V.unsafeCopy (V.slice 0 toShift samples) (V.slice (cursor - 1) toShift samples)
                writeIORef cursorRef toShift
            else
                writeIORef cursorRef 0
    _ -> error "Unsupported audio format"

onDrawFrame :: FrameState -> Texture -> Renderer -> Bus -> IO Bus
onDrawFrame frame texture renderer bus = do
    bs <- runRender (render bus R.>> toSDL2ByteString) frame
    updateTexture texture Nothing bs (256 * 3)
    copy renderer texture Nothing Nothing
    present renderer
    snd <$> runBusM bus handleEvents

-- {-# INLINE getCPUTimeUs #-}
-- getCPUTimeUs :: IO Double
-- getCPUTimeUs = (/ 1000000) . fromIntegral <$> getCPUTime
