module GameEngineServer.State.Scene.Nameable (Nameable (..)) where


class Nameable a where
    setName :: a -> String -> a
    getName :: a -> String