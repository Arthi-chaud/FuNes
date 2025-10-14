module Nes.Render (
    render,
) where

import Data.Vector.Mutable
import Nes.Bus
import Nes.Render.Background
import Nes.Render.Frame
import Nes.Render.Monad
import qualified Nes.Render.Monad as Render
import Nes.Render.Sprite

render :: Bus -> Render DirtyFrame Rendered r ()
render bus = Render.do
    clearAll
    renderBackground bus
    renderSprites bus
    applySprites
    renderPixels

clearAll :: Render a DirtyFrame r ()
clearAll = Render.do
    withFrameState $ \st -> do
        set (unBuffer $ pixelBuffer st) ((0, 0, 0), TransparentBG)
        set (unBuffer $ spriteBuffer st) Nothing
    unsafeStep

-- | Pulls pixels from 'pixelBuffer' to 'sdl2Frame'
renderPixels :: Render Renderable Rendered r ()
renderPixels = Render.do
    withFrameState $ \st -> iforM_ (unBuffer $ pixelBuffer st) $ \offset (color, _) ->
        let coord = (offset `mod` frameWidth, offset `div` frameWidth)
         in frameSetPixel color coord (sdl2Frame st)
    unsafeStep
