module GameEngineServer.State.Scene.Model.Geometry (Geometry (..), Face (..)) where

import Linear.V3 (V3)


data Geometry =
    -- |Geometry made up of 'Polygon's
    Polygonal [Face]


-- Polygonal

-- |Represents one face of a 'Polygonal'
data Face =
    -- |Represents one face of a 'Polygonal'
    Face {
        faceVertices :: [V3 Double],
        faceNormal :: V3 Double
    }