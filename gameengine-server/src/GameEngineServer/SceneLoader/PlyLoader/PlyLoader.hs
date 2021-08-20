{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.PlyLoader.PlyLoader (loadPolygonalFromPLY) where

import Data.ByteString (ByteString)

import GameEngineServer.State.Scene.Model.Geometry (Geometry)
import GameEngineServer.SceneLoader.PlyLoader.PlyGeometryGenerator (generateGeometry)
import GameEngineServer.SceneLoader.PlyLoader.PlyParser (plyParser)
import GameEngineServer.SceneLoader.PlyLoader.PlyFileLoader (PLYFileGenerationError)
import GameEngineServer.SceneLoader.PlyLoader.PlyFileLoader (generatePLYFile)


-- |Load a 'Polygonal' 'Geometry' from a PLY file given as a 'ByteString'
loadPolygonalFromPLY :: ByteString -> Either Geometry (Either String PLYFileGenerationError)
loadPolygonalFromPLY file = case generatePLYFile $ plyParser file of
    Left plyFile -> generateGeometry plyFile
    Right genError -> Right $ Right genError