{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStrict #-}

-- Using laziness to build cyclic lists for the output

module Nes.APU.State.Filter.Fir (FirFilter (..), lowPassFirFilter) where

import Data.Functor ((<&>))
import qualified Data.Vector.Unboxed as V
import Nes.APU.State.Filter.Class
import Nes.APU.State.Filter.Constants

-- | Finite impulse response (FIR) filter
data FirFilter = MkFirF
    { kernel :: !(V.Vector Float)
    , inputs :: !(V.Vector Float)
    , inputIndex :: {-# UNPACK #-} !Int
    }

instance Filter FirFilter where
    output f =
        let
            kernelL = V.toList $ kernel f
            inputsL = drop (inputIndex f) $ Prelude.cycle $ V.toList $ inputs f
         in
            sum (zipWith (*) kernelL inputsL)
    consume sample f =
        f
            { inputIndex = newInputIndex
            , inputs = newInputs
            }
      where
        newInputIndex = (inputIndex f + 1) `mod` V.length (inputs f)
        newInputs = inputs f V.// [(inputIndex f, sample)]

lowPassFirFilter :: SampleRate -> Cutoff -> Int -> FirFilter
lowPassFirFilter sampleRate cutoff windowSize = MkFirF{..}
  where
    inputIndex = 0
    inputs = V.replicate (windowSize + 1) 0
    kernel = windowedSincKernel sampleRate cutoff windowSize

windowedSincKernel :: SampleRate -> Cutoff -> Int -> V.Vector Float
windowedSincKernel sampleRate cutoff windowSize =
    let
        fc = cutoff / sampleRate
        kernelL :: [Float]
        kernelL =
            [0 .. windowSize] <&> \i ->
                (sinc i fc windowSize) * blackmanWindow i windowSize
        kernelV = V.fromList kernelL
     in
        normalise kernelV
  where
    blackmanWindow :: Int -> Int -> Float
    blackmanWindow idx winSize =
        let
            fIdx = fromIntegral idx
            fWinSize = fromIntegral winSize
            tau = 2 * pi
         in
            0.42
                - 0.5
                    * ((cos ((tau * fIdx) / fWinSize)) + 0.08 * (cos ((2 * tau * fIdx / fWinSize))))
    sinc :: Int -> Float -> Int -> Float
    sinc idx fc winSize =
        let
            fIdx = fromIntegral idx
            fWinSize = fromIntegral winSize
            shiftedIndex = fIdx - (fWinSize / 2)
            tau = 2 * pi
         in
            if idx == (windowSize `div` 2)
                then tau * fc
                else (mySin (tau * fc * shiftedIndex)) / shiftedIndex
    normalise :: V.Vector Float -> V.Vector Float
    normalise vec =
        let
            vecSum = V.sum vec
         in
            V.map (/ vecSum) vec

-- | Faster implementation of the sin function,
--
-- Stolen from https://www.youtube.com/watch?v=72dI7dB3ZvQ
mySin :: Float -> Float
mySin t =
    let
        j0 = t * 0.15915
        j1 = j0 - fromIntegral (floor j0 :: Int)
     in
        20.785 * j1 * (j1 - 0.5) * (j1 - 1)
