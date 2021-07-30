{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.ModelLoader (UnloadedModel (..), loadModel) where

import Prelude hiding (lookup)

import Data.Aeson (FromJSON, parseJSON, (.:), (.:?), withObject)

import Linear.V3 (V3 (V3))

import Data.Maybe (fromMaybe)
import Data.Map (Map, lookup, keys)

import Control.Monad (mapM)

import GameEngineServer.State.Scene.Model.Model (Model (..))
import GameEngineServer.SceneLoader.VectorLoader (V3)
import GameEngineServer.State.Scene.Model.Geometry (Geometry)


data UnloadedModel = UnloadedModel (V3 Double) (V3 Double) (V3 Double) String [String]

instance FromJSON UnloadedModel where
    parseJSON = withObject "UnloadedModel" $ \obj -> do
        identifierMaybe <- obj .:? "identifier"
        positionMaybe <- obj .:? "position"
        rotationMaybe <- obj .:? "rotation"
        scaleMaybe <- obj .:? "scale"

        singleGeomMaybe <- obj .:? "model"
        singleGeomsMaybe <- obj .:? "model"
        pluralGeomMaybe <- obj .:? "models"
        pluralGeomsMaybe <- obj .:? "models"

        let geometryNames = joinMaybe singleGeomMaybe $ joinMaybe pluralGeomMaybe $ (fromMaybe [] singleGeomsMaybe) ++ (fromMaybe [] pluralGeomsMaybe)

        return $ UnloadedModel (fromMaybe (V3 0 0 0) positionMaybe) (fromMaybe (V3 0 0 0) rotationMaybe) (fromMaybe (V3 0 0 0) scaleMaybe) (fromMaybe "" identifierMaybe) geometryNames

joinMaybe :: Maybe a -> [a] -> [a]
joinMaybe (Just x) xs = x:xs
joinMaybe Nothing xs = xs

-- |Look up the unloaded geometry of an 'UnloadedModel', turning it into a 'Model' (failing if any model strings can't be resolved)
loadModel :: Map String Geometry -> UnloadedModel -> Maybe Model
loadModel loadedGeometry (UnloadedModel position rotation scale identifier geometryNames) = do
    geometries <- mapM (\name -> lookup name loadedGeometry) geometryNames
    return $ Model position rotation scale identifier geometries