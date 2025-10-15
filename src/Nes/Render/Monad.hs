{-# LANGUAGE RankNTypes #-}

module Nes.Render.Monad (
    -- * Monad
    Render (..),
    RenderStep (..),
    runRender,
    withFrameState,
    modifyFrameState,
    (>>=),
    (>>),
    return,

    -- * Operations
    whenR,
    liftIO,
    unsafeStep,
    unsafeCastRender,
    toSDL2ByteString,
) where

import qualified Control.Monad as IO
import Data.ByteString.Internal
import Foreign (castForeignPtr)
import Nes.Render.Frame
import Prelude hiding (return, (>>), (>>=))

data RenderStep = DirtyFrame | BGDrawn | BGAndSpritesDrawn | Renderable | Rendered deriving (Eq)

-- | An operation that updates a 'FrameStep'
newtype Render (s0 :: RenderStep) (s1 :: RenderStep) r a = MkRender {unRender :: FrameState -> (FrameState -> a -> IO r) -> IO r} deriving (Functor)

instance Applicative (Render s0 s1 r) where
    pure a = MkRender $ \st cont -> cont st a
    liftA2 f (MkRender a) (MkRender b) = MkRender $ \st cont ->
        a st $ \st' a' ->
            b st' $ \st'' b' ->
                cont st'' (f a' b')

{-# INLINE runRender #-}
runRender :: Render DirtyFrame Rendered r r -> FrameState -> IO r
runRender (MkRender f) st = f st $ \_ res -> pure res

{-# INLINE withFrameState #-}
withFrameState :: (FrameState -> IO a) -> Render b b r a
withFrameState f = MkRender $ \st cont -> f st IO.>>= \(!i) -> cont st i

{-# INLINE modifyFrameState #-}
modifyFrameState :: (FrameState -> FrameState) -> Render b b r ()
modifyFrameState f = MkRender $ \st cont -> cont (f st) ()

{-# INLINE toSDL2ByteString #-}
toSDL2ByteString :: Render Rendered Rendered r ByteString
toSDL2ByteString = MkRender $ \st cont -> cont st (BS (castForeignPtr $ sdl2Frame st) frameLength)

{-# INLINE liftIO #-}
liftIO :: IO a -> Render b b r a
liftIO io = MkRender $ \st cont -> io IO.>>= \(!i) -> cont st i

{-# INLINE whenR #-}
whenR :: Bool -> Render a a r () -> Render a a r ()
whenR cond (MkRender f) = MkRender $ \st cont -> if cond then f st cont else cont st ()

{-# INLINE unsafeCastRender #-}
unsafeCastRender :: Render a b r c -> Render a' b' r c
unsafeCastRender (MkRender f) = MkRender f

{-# INLINE unsafeStep #-}
unsafeStep :: forall b a r. Render a b r ()
unsafeStep = MkRender $ \st cont -> cont st ()

{-# INLINE (>>=) #-}
(>>=) :: Render a b r s -> (s -> Render b c r s') -> Render a c r s'
(MkRender f) >>= f' = MkRender $ \st cont -> f st $ \st' !s -> unRender (f' s) st' cont

{-# INLINE (>>) #-}
(>>) :: Render a b r s -> Render b c r s' -> Render a c r s'
(MkRender f) >> f' = MkRender $ \st cont -> f st $ \st' !_ -> unRender f' st' cont

{-# INLINE return #-}
return :: a -> Render b b r a
return a = MkRender $ \st cont -> cont st a
