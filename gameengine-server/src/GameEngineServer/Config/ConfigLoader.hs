module GameEngineServer.Config.ConfigLoader (loadConfig, loadConfigString) where

import Prelude hiding (readFile)

import System.Directory (Permissions (readable), doesFileExist, copyFile, getPermissions, getCurrentDirectory)
import System.FilePath ((</>))

import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString, readFile)

import GameEngineServer.Config.Config (Config (..), CfgLevel (..))


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
                    return $ Just $ fileContents
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