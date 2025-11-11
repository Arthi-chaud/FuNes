module Nes.APU.Monad (
    APU (..),
    runAPU,
    modifyAPUState,
    modifyAPUStateWithSideEffect,
    withAPUState,
    setSideEffect,
    withSideEffect,
) where

import Control.Monad.IO.Class
import Nes.APU.State
import Nes.Bus.SideEffect

newtype APU r a = MkAPU
    { unAPU :: APUState -> CPUSideEffect -> (APUState -> CPUSideEffect -> a -> IO r) -> IO r
    }
    deriving (Functor)

instance Applicative (APU r) where
    {-# INLINE pure #-}
    pure a = MkAPU $ \(!st) (!cpuEff) cont -> cont st cpuEff a

    {-# INLINE liftA2 #-}
    liftA2 f (MkAPU a) (MkAPU b) = MkAPU $ \(!st) (!cpuEff) cont ->
        a st cpuEff $ \(!st') (!cpuEff') !a' -> b st' cpuEff' $ \(!st'') (!cpuEff'') !b' -> cont st'' cpuEff'' (f a' b')

instance Monad (APU r) where
    {-# INLINE (>>=) #-}
    (MkAPU a) >>= next = MkAPU $ \(!st) !cpuEff cont ->
        a st cpuEff $ \(!st') (!cpuEff') (!a') -> unAPU (next a') st' (cpuEff <> cpuEff') cont

instance MonadIO (APU r) where
    {-# INLINE liftIO #-}
    liftIO io = MkAPU $ \st cpuEff cont -> io >>= cont st cpuEff

instance MonadFail (APU r) where
    {-# INLINE fail #-}
    fail = liftIO . fail

{-# INLINE runAPU #-}
runAPU :: APUState -> APU (a, APUState, CPUSideEffect) a -> IO (a, APUState, CPUSideEffect)
runAPU !st f = unAPU f st mempty $ \(!st') (!cpuEff) a -> return (a, st', cpuEff)

{-# INLINE modifyAPUState #-}
modifyAPUState :: (APUState -> APUState) -> APU r ()
modifyAPUState f = MkAPU $ \(!st) (!cpuEff) cont -> cont (f st) cpuEff ()

{-# INLINE modifyAPUStateWithSideEffect #-}
modifyAPUStateWithSideEffect :: (APUState -> (APUState, CPUSideEffect)) -> APU r ()
modifyAPUStateWithSideEffect f = MkAPU $ \(!st) !cpuEff cont ->
    let (st', sideEff) = f st in cont st' (cpuEff <> sideEff) ()

{-# INLINE withAPUState #-}
withAPUState :: (APUState -> a) -> APU r a
withAPUState f = MkAPU $ \(!st) !cpuEff cont -> cont st cpuEff (f st)

{-# INLINE setSideEffect #-}
setSideEffect :: (CPUSideEffect -> CPUSideEffect) -> APU r ()
setSideEffect f = MkAPU $ \(!st) !cpuEff cont -> cont st (f cpuEff) ()

{-# INLINE withSideEffect #-}
withSideEffect :: (CPUSideEffect -> a) -> APU r a
withSideEffect f = MkAPU $ \(!st) !cpuEff cont -> cont st cpuEff (f cpuEff)
