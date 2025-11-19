{-# LANGUAGE RecordWildCards #-}

module Nes.APU.State.Filter.Thread (
    -- * Definition
    FilterThread (..),

    -- * Constructor
    newNoopFilterThread,
    newFilterThread,

    -- * Destructor
    killFilterThread,
) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Nes.APU.State.Filter.Chain
import Nes.APU.State.Filter.Class
import Nes.APU.State.Filter.Constants

data FilterThread = MkFT
    { consumeSample :: Sample -> IO ()
    , outputSample :: !(IO Sample)
    , threadId :: {-# UNPACK #-} !(Maybe ThreadId)
    }

-- | A FilterThread that does nothing and always returns 0
newNoopFilterThread :: FilterThread
newNoopFilterThread = MkFT (const $ pure ()) (pure 0) Nothing

-- | A FilterThread that spawns a process to run the filter chain in the backgroun
newFilterThread :: IO FilterThread
newFilterThread = do
    filtersRef <- newIORef =<< newFilterChain defaultOutputRate
    inputVar <- newIORef Nothing
    getInputVar <- newIORef False
    postOutputVar <- newEmptyMVar
    threadId <- Just <$> forkIO (thread filtersRef inputVar getInputVar postOutputVar)
    let consumeSample = writeIORef inputVar . Just
        outputSample = writeIORef getInputVar True >> takeMVar postOutputVar
    return $ MkFT{..}

thread :: IORef FilterChain -> IORef (Maybe Sample) -> IORef Bool -> MVar Sample -> IO ()
thread filterRef inV getV postV = do
    msample <- readIORef inV
    case msample of
        Nothing -> pure ()
        Just sample -> readIORef filterRef >>= consume sample >>= writeIORef filterRef

    needOutput <- readIORef getV
    when needOutput $ do
        filterOutput <- output =<< readIORef filterRef
        postIsFull <- tryPutMVar postV filterOutput
        when postIsFull $ return () -- NOTE Shouldn't happen
    thread filterRef inV getV postV

killFilterThread :: FilterThread -> IO ()
killFilterThread ft = maybe (pure ()) killThread $ threadId ft
