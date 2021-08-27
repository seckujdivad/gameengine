module GameEngineServer.Network.ModelVectorProperty (ModelVectorProperty (..)) where


-- |Represents a property of a model (for broadcasting updates to these properties)
data ModelVectorProperty =
    Position
    | Rotation
    | Scale
    deriving (Enum, Show)