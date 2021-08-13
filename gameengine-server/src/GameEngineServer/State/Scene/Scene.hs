module GameEngineServer.State.Scene.Scene (Scene (..)) where

import GameEngineServer.State.Scene.Model.Model (Model (..))


-- |Represents a Scene (that is rendered by clients)
data Scene =
    -- |Represents a Scene (that is rendered by clients)
    Scene {
        -- |'Model's contained by the scene
        scnModels :: [Model]
    } deriving (Show)