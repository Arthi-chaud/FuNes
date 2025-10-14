module Nes.Render (
    render,
) where

import Nes.Bus
import Nes.Render.Background
import Nes.Render.Frame2
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
    forR [0 .. frameWidth * frameHeight - 1] $ \offset -> Render.do
        let coord = (offset `mod` frameWidth, offset `div` frameWidth)
        withFrameState $ frameSetPixel (0, 0, 0) coord . sdl2Frame
    forR [0 .. bufferLength - 1] $ \offset -> Render.do
        withFrameState $ bufferSetOffset ((0, 0, 0), TransparentBG) offset . pixelBuffer
        withFrameState $ bufferSetOffset Nothing offset . spriteBuffer
    unsafeStep

-- | Pulls pixels from 'pixelBuffer' to 'sdl2Frame'
renderPixels :: Render Renderable Rendered r ()
renderPixels = Render.do
    forR [0 .. frameWidth * frameHeight - 1] $ \offset -> Render.do
        let coord = (offset `mod` frameWidth, offset `div` frameWidth)
        (color, _) <- withFrameState $ bufferGet coord . pixelBuffer
        withFrameState $ frameSetPixel color coord . sdl2Frame
    unsafeStep
