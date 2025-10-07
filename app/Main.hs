module Main (main) where

import Control.Monad
import qualified Data.ByteString.Internal as BS
import Events
import Foreign (castForeignPtr)
import Nes.Bus
import Nes.Bus.Monad (runBusM)
import Nes.CPU.Interpreter
import Nes.Rom
import Render
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
    bus <- newBus rom (onDrawFrame frame texture renderer)
    void $ runProgram bus (pure ())
    destroyRenderer renderer

onDrawFrame :: Frame -> Texture -> Renderer -> Bus -> IO Bus
onDrawFrame frame texture renderer bus = do
    render frame bus
    let bs = BS.fromForeignPtr0 (castForeignPtr $ unF frame) frameLength
    updateTexture texture Nothing bs (256 * 3)
    copy renderer texture Nothing Nothing
    present renderer
    snd <$> runBusM bus handleEvents
