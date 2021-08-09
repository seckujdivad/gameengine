module GameEngineServer.State.Scene.Model.Geometry (Geometry (..), Face (..), invertNormals) where

import Linear.V3 (V3)


-- |A data type for storing geometry
data Geometry =
    -- |Geometry made up of 'Polygon's
    Polygonal [Face]

-- |Invert the normals of a 'Geometry'
invertNormals :: Geometry -> Geometry
invertNormals (Polygonal faces) = Polygonal (map (\face -> face {faceNormal = 0 - faceNormal face}) faces)

-- |Represents one face of a 'Polygonal'
data Face =
    -- |Represents one face of a 'Polygonal'
    Face {
        faceVertices :: [V3 Double],
        faceNormal :: V3 Double
    }