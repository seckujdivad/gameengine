module GameEngineServer.State.Scene.Rotatable (Rotatable (..)) where

import Linear.V3 (V3)


class Rotatable a where
    setRotation :: a -> V3 Double -> a
    getRotation :: a -> V3 Double