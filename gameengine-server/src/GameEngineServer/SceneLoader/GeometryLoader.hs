module GameEngineServer.SceneLoader.GeometryLoader () where

import Data.Map (Map, empty)

import Data.Aeson (FromJSON (..), (.:), (.:?), withObject)

import GameEngineServer.State.Scene.Model.Geometry (Geometry (..))


-- |Lookup table for mapping geometry names (found in scene files) to loaded 'Geometry'
newtype GeometryTable = GeometryTable (Map String Geometry)

instance FromJSON GeometryTable where
    parseJSON = withObject "GeometryTable" $ \obj -> do
        return $ GeometryTable empty