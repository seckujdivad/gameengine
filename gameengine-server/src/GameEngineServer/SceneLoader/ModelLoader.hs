{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.ModelLoader (UnloadedModel (..), loadModel) where

import Prelude hiding (lookup)

import Data.Aeson (FromJSON, parseJSON, (.:?), withObject)

import Linear.V3 (V3 (V3))

import Data.Maybe (fromMaybe)

import Data.Map (lookup)

import Data.Text (pack)

import GameEngineServer.State.Scene.Model.Model (Model (..))
import GameEngineServer.SceneLoader.VectorLoader (extractJSONableV3)
import GameEngineServer.SceneLoader.GeometryLoader (GeometryTable (..))
import GameEngineServer.SceneLoader.ListOrValue (listOrValueToList)


-- |Information on how to load a model that hasn't yet been loaded
data UnloadedModel =
    -- |Information on how to load a model that hasn't yet been loaded
    UnloadedModel (V3 Double) (V3 Double) (V3 Double) [String]

instance FromJSON UnloadedModel where
    parseJSON = withObject "UnloadedModel" $ \obj -> do
        positionMaybeWrapped <- obj .:? "position"
        rotationMaybeWrapped <- obj .:? "rotation"
        scaleMaybeWrapped <- obj .:? "scale"

        let
            positionMaybe = fmap extractJSONableV3 positionMaybeWrapped
            rotationMaybe = fmap extractJSONableV3 rotationMaybeWrapped
            scaleMaybe = fmap extractJSONableV3 scaleMaybeWrapped

        singleGeoms <- obj .:? "model"
        pluralGeoms <- obj .:? "models"

        let geometryNames = (fromMaybe [] (fmap listOrValueToList singleGeoms)) ++ (fromMaybe [] (fmap listOrValueToList pluralGeoms))

        return $ UnloadedModel (fromMaybe (V3 0 0 0) positionMaybe) (fromMaybe (V3 0 0 0) rotationMaybe) (fromMaybe (V3 0 0 0) scaleMaybe) geometryNames

-- |Look up the unloaded geometry of an 'UnloadedModel', turning it into a 'Model' (failing if any model strings can't be resolved)
loadModel :: GeometryTable -> UnloadedModel -> Maybe Model
loadModel (GeometryTable loadedGeometry) (UnloadedModel position rotation scale geometryNames) = do
    geometries <- mapM (\name -> lookup (pack name) loadedGeometry) geometryNames
    return $ Model position rotation scale (concat geometries)