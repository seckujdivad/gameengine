{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.PlyLoader.PlyGeometryGenerator (generateGeometry) where


import GameEngineServer.State.Scene.Model.Geometry (Geometry (Polygonal), Face (Face))
import GameEngineServer.SceneLoader.PlyLoader.PlyFileLoader (generatePLYFile, PLYFileGenerationError (..), PLYFileGenerationErrorInner (..), PLYFile (..), Element (..), Property (..), Value (..))
import GameEngineServer.SceneLoader.PlyLoader.PlyParser (Parse)


-- |Generates 'Geometry' from a parsed PLY file in the form of a list of 'Parse' with line indices
generateGeometry :: [(Int, Parse)] -> Either Geometry PLYFileGenerationError
generateGeometry parses = Right $ PLYFileGenerationError Nothing UnexpectedEOF -- TODO
    where
        plyFileOrError = generatePLYFile parses