module GameEngineServer.State.Scene.Model.Model (Model (Model)) where

import Linear.V3 (V3)

import GameEngineServer.State.Scene.Positionable (Positionable (..))
import GameEngineServer.State.Scene.Rotatable (Rotatable (..))
import GameEngineServer.State.Scene.Scalable (Scalable (..))
import GameEngineServer.State.Scene.Nameable (Nameable (..))
import GameEngineServer.State.Scene.Model.Geometry (Geometry (..))


-- |Model in a scene
data Model =
    -- |Model in a scene
    Model {
        mdlPosition :: V3 Double,
        mdlRotation :: V3 Double,
        mdlScale :: V3 Double,
        mdlName :: String,
        mdlGeometries :: [Geometry]
    }

instance Positionable Model where
    setPosition model position = model {mdlPosition = position}
    getPosition = mdlPosition

instance Rotatable Model where
    setRotation model rotation = model {mdlRotation = rotation}
    getRotation = mdlRotation

instance Scalable Model where
    setScale model scale = model {mdlScale = scale}
    getScale = mdlScale

instance Nameable Model where
    setName model name = model {mdlName = name}
    getName = mdlName