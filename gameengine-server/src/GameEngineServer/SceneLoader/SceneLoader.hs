{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.SceneLoader () where

import Data.Aeson (FromJSON, parseJSON, (.:), withObject)

import GameEngineServer.State.Scene.Scene (Scene (..))
import GameEngineServer.SceneLoader.ModelLoader (UnloadedModel (..), loadModel)


--instance FromJSON Scene where
--    parseJSON = withObject "" $ \obj -> Scene []