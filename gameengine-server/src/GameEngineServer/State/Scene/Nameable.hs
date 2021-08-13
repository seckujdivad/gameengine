module GameEngineServer.State.Scene.Nameable (Nameable (..), getAllWithName) where


class Nameable a where
    setName :: a -> String -> a
    getName :: a -> String

-- |Get all 'Nameable's from a list that share the given name
getAllWithName :: Nameable a => String -> [a] -> [a]
getAllWithName name = filter (\nameable -> name == getName nameable)