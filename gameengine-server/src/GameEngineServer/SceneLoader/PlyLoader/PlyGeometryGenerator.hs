{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.PlyLoader.PlyGeometryGenerator (generateGeometry) where

import Linear.V3 (V3 (..))
import Linear.V2 (V2 (..))
import Linear.Metric (normalize)

import Data.Map (lookup)

import Data.Maybe (mapMaybe, fromMaybe)

import Data.List (genericIndex)

import GameEngineServer.State.Scene.Model.Geometry (Geometry (Polygonal), Face (..))
import GameEngineServer.SceneLoader.PlyLoader.PlyFileLoader (PLYFileGenerationError (..), PLYFile (..), Element (..), getDoubleFromElement, getIntegerListFromElement)


-- |Generates 'Polygonal' 'Geometry' from a parsed PLY file in the form of a list of 'Parse' with line indices
generateGeometry :: PLYFile -> Either Geometry (Either String PLYFileGenerationError)
generateGeometry (PLYFile elementMap) = case Data.Map.lookup "vertex" elementMap of
        Just verticesElements -> case Data.Map.lookup "face" elementMap of
            Just faceElements -> Left $ geometryFromValues vertices vertexIndices
                where
                    vertexIndices = mapMaybe (getIntegerListFromElement "vertex_indices") faceElements
                    vertices = mapMaybe plyVertexFromVertexElement verticesElements

            Nothing -> Right $ Left "PLY file must contain element \"vertex_indices\""
        Nothing -> Right $ Left "PLY file must contain element \"vertex\""

-- |Utility type that contains a vertex from the Blender layout of .PLY files
data PLYVertex =
    -- |Utility type that contains a vertex from the Blender layout of .PLY files
    PLYVertex {
        -- |Position of the vertex
        plyvPos :: V3 Double,
        -- |Normal of the vertex
        plyvNormal :: V3 Double,
        -- |UV of the vertex
        plyvUV :: V2 Double
    }

-- |Convert an element that represents a "vertex" element in Blender's .PLY file layout into a 'PLYVertex'. Returns 'Nothing' if any properties are missing (apart from "s" and "t") or they are not floating point
plyVertexFromVertexElement :: Element -> Maybe PLYVertex
plyVertexFromVertexElement element = do
    x <- getDoubleFromElement "x" element
    y <- getDoubleFromElement "y" element
    z <- getDoubleFromElement "z" element
    nx <- getDoubleFromElement "nx" element
    ny <- getDoubleFromElement "ny" element
    nz <- getDoubleFromElement "nz" element
    let
        s = fromMaybe 0 $ getDoubleFromElement "s" element
        t = fromMaybe 0 $ getDoubleFromElement "t" element
    return PLYVertex {plyvPos = V3 x y z, plyvNormal = V3 nx ny nz, plyvUV = V2 s t}

-- |Generate a 'Polygonal' from a list of 'PLYVertex' and a list of faces, each represented as a list of vertex indices
geometryFromValues :: [PLYVertex] -> [[Integer]] -> Geometry
geometryFromValues allVertices faceVertexIndices = Polygonal faces
    where
        faces = map (\indices -> faceFromValues $ map (genericIndex allVertices) indices) faceVertexIndices

-- |Generate a 'Face' from a list of 'PLYVertex'
faceFromValues :: [PLYVertex] -> Face
faceFromValues vertices = Face {faceNormal = normalize normal, faceVertices = map plyvPos vertices}
    where
        normal = foldr (\left right -> plyvNormal left + right) (V3 0 0 0) vertices