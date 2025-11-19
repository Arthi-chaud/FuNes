{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStrict #-}

-- Using laziness to build cyclic lists for the output

module Nes.APU.State.Filter.Fir (FirFilter (..), lowPassFirFilter) where

import Control.Monad.IO.Class
import qualified Data.Vector.Unboxed.Mutable as V
import Nes.APU.State.Filter.Class
import Nes.APU.State.Filter.Constants

-- | Finite impulse response (FIR) filter
data FirFilter = MkFirF
    { kernel :: !(V.IOVector Float)
    , inputs :: !(V.IOVector Float)
    , inputIndex :: {-# UNPACK #-} !Int
    }

instance (MonadIO m) => Filter m FirFilter where
    output MkFirF{..} =
        liftIO $
            V.ifoldM'
                ( \acc idx n -> do
                    n' <- V.read inputs ((idx + inputIndex) `mod` V.length inputs)
                    return $ acc + (n * n')
                )
                0
                kernel
    consume sample f = liftIO $ do
        V.write (inputs f) (inputIndex f) sample
        return $
            f
                { inputIndex = newInputIndex
                }
      where
        newInputIndex = (inputIndex f + 1) `mod` V.length (inputs f)

lowPassFirFilter :: SampleRate -> Cutoff -> Int -> IO FirFilter
lowPassFirFilter sampleRate cutoff windowSize = do
    let inputIndex = 0
    kernel <- windowedSincKernel sampleRate cutoff (windowSize + 1)
    inputs <- V.replicate (windowSize + 1) 0
    return MkFirF{..}

windowedSincKernel :: SampleRate -> Cutoff -> Int -> IO (V.IOVector Float)
windowedSincKernel sampleRate cutoff windowSize = do
    let fc = cutoff / sampleRate
    kernelV <- V.generate windowSize $ \i -> (sinc i fc windowSize) * blackmanWindow i windowSize
    normalise kernelV
    return kernelV
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
    normalise :: V.IOVector Float -> IO ()
    normalise vec = do
        vecSum <- V.foldl' (+) 0 vec
        V.imapM_ (\i a -> V.write vec i (a / vecSum)) vec

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
