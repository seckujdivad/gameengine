{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.SceneLoader (loadScene) where

import Prelude hiding (readFile)

import Data.Aeson (FromJSON, parseJSON, (.:), withObject, eitherDecode, Value)
import Data.Aeson.Types (Parser)

import System.FilePath ((</>))

import Data.ByteString.Lazy (readFile)

import Data.HashMap.Strict (toList)

import Data.Text (Text)

import Data.Map (Map, empty, insert, mapMaybe)

--import Data.Attoparsec.ByteString (Parser)

import GameEngineServer.State.Scene.Scene (Scene (..))
import GameEngineServer.SceneLoader.ModelLoader (UnloadedModel (..), loadModel)
import GameEngineServer.SceneLoader.GeometryLoader (GeometryInfoTable (..), createGeometryTable)


-- |Contains all the information taken from a scene file required to load a 'Scene'
data UnloadedScene =
    -- |Contains all the information taken from a scene file required to load a 'Scene'
    UnloadedScene (Map Text UnloadedModel) GeometryInfoTable

instance FromJSON UnloadedScene where
    parseJSON = withObject "UnloadedScene" $ \obj -> do
        modelConfigs <- obj .: "layout"
        modelValues <- withObject "UnloadedScene layout" (return . toList) modelConfigs

        let
            modelFolder :: (Text, Value) -> Parser (Map Text UnloadedModel) -> Parser (Map Text UnloadedModel)
            modelFolder (key, value) modelsMapParser = do
                modelsMap <- modelsMapParser
                model <- parseJSON value
                return $ insert key model modelsMap
        models <- foldr modelFolder (return empty) modelValues
        
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