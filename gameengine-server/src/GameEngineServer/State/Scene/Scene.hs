module GameEngineServer.State.Scene.Scene () where

import GameEngineServer.State.Scene.Model (Model (..))


-- |Represents a Scene (that is rendered by clients)
data Scene =
    -- |Represents a Scene (that is rendered by clients)
    Scene {
        scnModels :: [Model]
    }