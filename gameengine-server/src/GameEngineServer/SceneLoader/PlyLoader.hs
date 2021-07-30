module GameEngineServer.SceneLoader.PlyLoader (loadPolygonalFromPLY) where

import Data.ByteString.Lazy (ByteString)

import GameEngineServer.State.Scene.Model.Model (Model (..))
import GameEngineServer.State.Scene.Model.Geometry (Geometry (Polygonal), Face (Face))


loadPolygonalFromPLY :: ByteString -> Geometry
loadPolygonalFromPLY file = Polygonal []