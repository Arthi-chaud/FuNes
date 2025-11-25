{-# LANGUAGE FlexibleInstances #-}

module Nes.APU.Monad (
    APU (..),
    runAPU,
    modifyDMCAndInterrupt,
) where

import Control.Monad.IO.Class
import Nes.APU.State
import Nes.APU.State.DMC
import Nes.Internal.MonadState
import Nes.Interrupt

newtype APU r a = MkAPU
    { unAPU :: APUState -> InterruptStatus -> (APUState -> InterruptStatus -> a -> IO r) -> IO r
    }
    deriving (Functor)

instance Applicative (APU r) where
    {-# INLINE pure #-}
    pure a = MkAPU $ \(!st) (!interr) cont -> cont st interr a

    {-# INLINE liftA2 #-}
    liftA2 f (MkAPU a) (MkAPU b) = MkAPU $ \(!st) (!interr) cont ->
        a st interr $ \(!st') (!interr') !a' -> b st' interr' $ \(!st'') (!interr'') !b' -> cont st'' interr'' (f a' b')

instance Monad (APU r) where
    {-# INLINE (>>=) #-}
    (MkAPU a) >>= next = MkAPU $ \st interr cont ->
        a st interr $ \(!st') (!interr') (!a') -> unAPU (next a') st' interr' cont

instance MonadIO (APU r) where
    {-# INLINE liftIO #-}
    liftIO io = MkAPU $ \st interr cont -> io >>= cont st interr

instance MonadFail (APU r) where
    {-# INLINE fail #-}
    fail = liftIO . fail

{-# INLINE runAPU #-}
runAPU :: APUState -> InterruptStatus -> APU (a, APUState, InterruptStatus) a -> IO (a, APUState, InterruptStatus)
runAPU !st !s f = unAPU f st s $ \(!st') (!interr) a -> return (a, st', interr)

instance MonadState APUState (APU r) where
    {-# INLINE get #-}
    get = MkAPU $ \st interr cont -> cont st interr st
    {-# INLINE set #-}
    set st' = MkAPU $ \_ (!interr) cont -> cont st' interr ()

instance MonadState InterruptStatus (APU r) where
    {-# INLINE get #-}
    get = MkAPU $ \st interr cont -> cont st interr interr
    {-# INLINE set #-}
    set interr' = MkAPU $ \st _ cont -> cont st interr' ()

{-# INLINE modifyDMCAndInterrupt #-}
modifyDMCAndInterrupt :: (DMC -> InterruptStatus -> (DMC, InterruptStatus)) -> APU r ()
modifyDMCAndInterrupt f = do
    dmc' <- use dmc
    i <- get
    let (dmc'', i') = f dmc' i
    dmc .= dmc''
    set i'
