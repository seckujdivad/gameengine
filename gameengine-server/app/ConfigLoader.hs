{-# LANGUAGE OverloadedStrings #-}

module ConfigLoader (loadConfig, loadConfigString, Config (..), CfgLevel (..)) where

import Prelude hiding (readFile)

import System.Directory (Permissions (readable), doesFileExist, copyFile, getPermissions, getCurrentDirectory)
import System.FilePath ((</>))

import Data.Aeson (FromJSON, parseJSON, (.:), withObject, decode, encode)
import Data.ByteString (readFile)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.ByteString.Lazy.Char8 (pack, unpack)


configPathRelative :: FilePath
configPathRelative = "settings.json"

configPath :: IO FilePath
configPath = getCurrentDirectory >>= (\root -> return $ root </> configPathRelative)

defaultConfigPathRelative :: FilePath
defaultConfigPathRelative = "settings.default.json"

defaultconfigPath :: IO FilePath
defaultconfigPath = getCurrentDirectory >>= (\root -> return $ root </> configPathRelative)

loadConfigString :: IO (Maybe ByteString)
loadConfigString = do
    customConfig <- loadCustomConfig
    case customConfig of
        Just _ -> return customConfig
        Nothing -> do
            defaultConfigPath <- defaultconfigPath
            defaultConfigExists <- doesFileExist defaultConfigPath
            if defaultConfigExists then do
                configPath <- configPath
                copyFile defaultConfigPath configPath
                loadCustomConfig
            else
                return Nothing
    
    where
        loadCustomConfig :: IO (Maybe ByteString)
        loadCustomConfig = do
            configPath <- configPath
            configExists <- doesFileExist configPath
            if configExists then do
                permissions <- getPermissions configPath
                if readable permissions then do
                    fileContents <- readFile configPath
                    return $ Just $ fromStrict fileContents
                else
                    return Nothing
            else
                return Nothing

data Config = Config {cfgInitialScene :: CfgLevel}

instance FromJSON Config where
    parseJSON = withObject "Config" $ \obj -> Config
        <$> obj .: "initial scene"

instance Show Config where
    show (Config initialScene) = "Config {initial scene: " ++ show initialScene ++ "}"

data CfgLevel = CfgLevel {cfglvlRoot :: String, cfglvlFile :: String}

instance FromJSON CfgLevel where
    parseJSON = withObject "CfgLevel" $ \obj -> CfgLevel
        <$> obj .: "root"
        <*> obj .: "file"

instance Show CfgLevel where
    show (CfgLevel root file) = "Config {root: \"" ++ root ++ "\", file: \"" ++ file ++ "\"}"

loadConfig :: IO (Maybe Config)
loadConfig = do
    configTextMaybe <- loadConfigString
    case configTextMaybe of
        Just configText -> do
            return (decode configText :: Maybe Config)
        Nothing -> return Nothing