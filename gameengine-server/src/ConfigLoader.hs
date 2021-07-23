{-# LANGUAGE OverloadedStrings #-}

module ConfigLoader (loadConfig, loadConfigString, Config (..), CfgLevel (..)) where

import Prelude hiding (readFile)

import System.Directory (Permissions (readable), doesFileExist, copyFile, getPermissions, getCurrentDirectory)
import System.FilePath ((</>))

import Data.Aeson (FromJSON, parseJSON, (.:), withObject, decode, encode)
import Data.ByteString (readFile)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.ByteString.Lazy.Char8 (pack, unpack)


{-|
Load the 'Data.ByteString.Lazy.ByteString' that contains the JSON file in the app root.

If the normal config file exists, it will be loaded. If not, the default config file will be copied to where the normal one should be and the normal one is loaded.
-}
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

{-|
Load the 'Config' data from the JSON file in the app root.

If the normal config file exists, it will be loaded. If not, the default config file will be copied to where the normal one should be and the normal one is loaded.
-}
loadConfig :: IO (Maybe Config)
loadConfig = do
    configTextMaybe <- loadConfigString
    case configTextMaybe of
        Just configText -> do
            return (decode configText)
        Nothing -> return Nothing

-- |Relative path to the config file (relative to the app root)
configPathRelative :: FilePath
configPathRelative = "settings.json"

-- |Absolute path to the config file
configPath :: IO FilePath
configPath = getCurrentDirectory >>= (\root -> return $ root </> configPathRelative)

-- |Relative path to the default config file (relative to the app root)
defaultConfigPathRelative :: FilePath
defaultConfigPathRelative = "settings.default.json"

-- |Absolute path to the default config file
defaultconfigPath :: IO FilePath
defaultconfigPath = getCurrentDirectory >>= (\root -> return $ root </> defaultConfigPathRelative)

-- |Stores configuration options for the server
data Config =
    -- |Stores configuration options for the server
    Config {
        -- |Information on the scene to load when the server loads
        cfgInitialScene :: CfgLevel
    }

instance FromJSON Config where
    parseJSON = withObject "Config" $ \obj -> Config
        <$> obj .: "initial scene"

instance Show Config where
    show (Config initialScene) = "Config {initial scene: " ++ show initialScene ++ "}"

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