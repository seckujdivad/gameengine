module GameEngineServer.State.Scene.Model.Geometry (Geometry (..), Face (..), invertNormals, snapVerticesToGrid) where

import Linear.V3 (V3 (..))


-- |A data type for storing geometry
data Geometry =
    -- |Geometry made up of 'Polygon's
    Polygonal [Face]

-- |Invert the normals of a 'Geometry'
invertNormals :: Geometry -> Geometry
invertNormals (Polygonal faces) = Polygonal (map (\face -> face {faceNormal = 0 - faceNormal face}) faces)

-- |Align all vertices in a 'Geometry' to a grid, where the size of the increments are given by a 'V3'
snapVerticesToGrid :: V3 Double -> Geometry -> Geometry
snapVerticesToGrid gridSize (Polygonal faces) = Polygonal (map (\face -> face {faceVertices = map (snapVertexToGrid gridSize) (faceVertices face)}) faces)

-- |Ensure each component of a 'V3' is a multiple of the corresponding component of another 'V3', rounding if not
snapVertexToGrid :: RealFrac a => V3 a -> V3 a -> V3 a
snapVertexToGrid gridSize vertex = V3 (roundToIncrement gsX vX) (roundToIncrement gsY vY) (roundToIncrement gsZ vZ)
    where
        V3 gsX gsY gsZ = gridSize
        V3 vX vY vZ = vertex

-- |Round a value to the nearest multiple of another
roundToIncrement :: RealFrac a => a -> a -> a
roundToIncrement increment value = (fpRound (value * increment)) / increment

-- |Floating point rounding of 'RealFrac's (i.e. 'Double' and 'Float'). Inefficient, but the best implementation I can come up with.
fpRound :: (RealFrac a, Num b) => a -> b
fpRound x = fromIntegral (round x :: Integer)

-- |Represents one face of a 'Polygonal'
data Face =
    -- |Represents one face of a 'Polygonal'
    Face {
        -- |Vertices that make up the face
        faceVertices :: [V3 Double],
        -- |Normal vector of the face
        faceNormal :: V3 Double
    }