module GameEngineServer.State.Scene.Scene (Scene (..)) where

import GameEngineServer.State.Scene.Model.Model (Model (..))

import Data.Map.Strict (Map)

import Data.Text (Text)


-- |Represents a Scene (that is rendered by clients)
data Scene =
    -- |Represents a Scene (that is rendered by clients)
    Scene {
        -- |'Model's contained by the scene
        scnModels :: Map Text Model
    } deriving (Show)