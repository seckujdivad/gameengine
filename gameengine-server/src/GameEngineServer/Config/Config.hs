{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.Config.Config (Config (..), CfgLevel (..)) where

import Data.Aeson (FromJSON, parseJSON, (.:), withObject)

-- |Stores configuration options for the server
data Config =
    -- |Stores configuration options for the server
    Config {
        -- |Information on the scene to load when the server loads
        cfgInitialScene :: CfgLevel,
        -- |Processing steps per second, > 0
        cfgTickrate :: Int
    }

instance FromJSON Config where
    parseJSON = withObject "Config" $ \obj -> Config
        <$> obj .: "initial scene"
        <*> obj .: "tickrate"

instance Show Config where
    show (Config initialScene tickrate) = "Config {initial scene: " ++ show initialScene ++ ", tickrate: " ++ show tickrate ++ "}"

-- |Stores information on a scene file
data CfgLevel =
    -- |Stores information on a scene file
    CfgLevel {
        -- |Root path to use when loading the scene file
        cfglvlRoot :: String,
        -- |Path to the scene file relative to the root path
        cfglvlFile :: String
    }

instance FromJSON CfgLevel where
    parseJSON = withObject "CfgLevel" $ \obj -> CfgLevel
        <$> obj .: "root"
        <*> obj .: "file"

instance Show CfgLevel where
    show (CfgLevel root file) = "Config {root: \"" ++ root ++ "\", file: \"" ++ file ++ "\"}"