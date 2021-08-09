{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.PlyLoader.PlyLoader (loadPolygonalFromPLY) where

import Data.ByteString.Lazy (ByteString)

import GameEngineServer.State.Scene.Model.Geometry (Geometry)
import GameEngineServer.SceneLoader.PlyLoader.PlyGeometryGenerator (generateGeometry)
import GameEngineServer.SceneLoader.PlyLoader.PlyParser (parser)
import GameEngineServer.SceneLoader.PlyLoader.PlyFileLoader (PLYFileGenerationError)


-- |Load a 'Polygonal' 'Geometry' from a PLY file given as a 'ByteString'
loadPolygonalFromPLY :: ByteString -> Either Geometry (Either String PLYFileGenerationError)
loadPolygonalFromPLY file = generateGeometry $ parser file