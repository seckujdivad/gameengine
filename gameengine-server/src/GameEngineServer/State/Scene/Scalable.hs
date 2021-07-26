module GameEngineServer.State.Scene.Scalable (Scalable (..)) where

import Linear.V3 (V3)


class Scalable a where
    setScale :: a -> V3 Double -> a
    getScale :: a -> V3 Double