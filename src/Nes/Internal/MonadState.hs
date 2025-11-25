module Nes.Internal.MonadState (
    -- * Definition
    MonadState (..),
    modify,
    modify',
    gets,
    getsM,

    -- * Lenses
    (.=),
    (+=),
    (-=),
    (%=),
    use,
    uses,
    usesM,
) where

import qualified Control.Lens as Lens

-- | Very similar to mtl's StateMonad, however we don't use functional dependencies to resolve the type of the state. Monads that have multiple instances for this class will have to use explicit type application
class (Monad m) => MonadState s m where
    get :: m s
    set :: s -> m ()

{-# INLINE modify #-}
modify :: (MonadState s m) => (s -> s) -> m ()
modify f = get >>= set . f

{-# INLINE modify' #-}
modify' :: (MonadState s m) => (s -> (a, s)) -> m a
modify' f = do
    (a, s) <- gets f
    set s
    return a

{-# INLINE gets #-}
gets :: (MonadState s m) => (s -> a) -> m a
gets f = fmap f get

{-# INLINE getsM #-}
getsM :: (MonadState s m) => (s -> m a) -> m a
getsM f = get >>= f

(.=) :: (MonadState s m) => Lens.ASetter' s a -> a -> m ()
(.=) l a = modify (l Lens..~ a)

(+=) :: (MonadState s m, Num a) => Lens.ASetter' s a -> a -> m ()
(+=) l a = modify (l Lens.+~ a)

(-=) :: (MonadState s m, Num a) => Lens.ASetter' s a -> a -> m ()
(-=) l a = modify (l Lens.-~ a)

(%=) :: (MonadState s m) => Lens.ASetter' s a -> (a -> a) -> m ()
(%=) l f = modify (l Lens.%~ f)

use :: (MonadState s m) => Lens.Getting a s a -> m a
use l = gets (Lens.view l)

uses :: (MonadState s m) => Lens.Getting a s a -> (a -> r) -> m r
uses l f = gets (f . Lens.view l)

-- | Like 'uses', but the computation can have side effect
usesM :: (MonadState s m) => Lens.Getting a s a -> (a -> m r) -> m r
usesM l f = gets (Lens.view l) >>= f
