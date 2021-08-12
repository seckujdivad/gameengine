{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.SceneLoader (loadScene) where

import Prelude hiding (readFile)

import Data.Aeson (FromJSON, parseJSON, (.:), withObject, withArray, eitherDecode)

import System.FilePath ((</>))

import Data.ByteString.Lazy (readFile)

import Data.Vector (toList)

import Data.Maybe (mapMaybe)

import GameEngineServer.State.Scene.Scene (Scene (..))
import GameEngineServer.SceneLoader.ModelLoader (UnloadedModel (..), loadModel)
import GameEngineServer.SceneLoader.GeometryLoader (GeometryInfoTable (..), createGeometryTable)


-- |Contains all the information taken from a scene file required to load a 'Scene'
data UnloadedScene =
    -- |Contains all the information taken from a scene file required to load a 'Scene'
    UnloadedScene [UnloadedModel] GeometryInfoTable

instance FromJSON UnloadedScene where
    parseJSON = withObject "UnloadedScene" $ \obj -> do
        modelConfigs <- obj .: "layout"
        modelValues <- withArray "UnloadedScene layout" (return . toList) modelConfigs
        models <- foldr (\value modelsParser -> do
            models <- modelsParser
            model <- parseJSON value
            return (model:models)) (return []) modelValues
        
        geometryInfoTable <- obj .: "models"

        return $ UnloadedScene models geometryInfoTable

-- |Load a 'Scene' from the disk given the root path and relative path to the scene file
loadScene :: FilePath -> FilePath -> IO (Maybe Scene)
loadScene rootPath configPath = do
    sceneConfig <- readFile $ rootPath </> configPath
    case eitherDecode sceneConfig of
        Left errorMsg -> do
            putStrLn errorMsg
            return Nothing
        Right unloadedScene -> do
            let UnloadedScene unloadedModels geometryInfoTable = unloadedScene
            geometryTable <- createGeometryTable geometryInfoTable rootPath
            return $ Just $ Scene (mapMaybe (loadModel geometryTable) unloadedModels)