module GameEngineServer.State.Scene.Positionable (Positionable (..)) where

import Linear.V3 (V3)


class Positionable a where
    setPosition :: a -> V3 Double -> a
    getPosition :: a -> V3 Double