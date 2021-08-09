{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.VectorLoader (V1, V2, V3, V4) where

import Data.Aeson (FromJSON, parseJSON, Value (..))
import Data.Aeson.Types (typeMismatch, Parser)

import Linear.V1 (V1 (V1))
import Linear.V2 (V2 (V2))
import Linear.V3 (V3 (V3))
import Linear.V4 (V4 (V4))


-- |Parse a fixed size array of 'Value'. If no array is present, only one 'Value', then parse that value and fill the array with it.
parseFixedSize :: FromJSON a => Int -> Value -> Parser [a]
parseFixedSize n (Array jsonValue) = do
    values <- parseJSON (Array jsonValue)
    case length values == n of
        True -> return values
        False -> typeMismatch (show n ++ "D vector") (Array jsonValue)
parseFixedSize n (Number jsonValue) = do
    value <- parseJSON (Number jsonValue)
    return $ map (const value) [1..n]
parseFixedSize n jsonValue = typeMismatch (show n ++ "D vector") jsonValue

instance FromJSON a => FromJSON (V1 a) where
    parseJSON jsonValue = do
        [x] <- parseFixedSize 1 jsonValue
        return $ V1 x

instance FromJSON a => FromJSON (V2 a) where
    parseJSON jsonValue = do
        xs <- parseFixedSize 2 jsonValue
        return $ V2 (xs !! 0) (xs !! 1)

instance FromJSON a => FromJSON (V3 a) where
    parseJSON jsonValue = do
        xs <- parseFixedSize 3 jsonValue
        return $ V3 (xs !! 0) (xs !! 1) (xs !! 2)

instance FromJSON a => FromJSON (V4 a) where
    parseJSON jsonValue = do
        xs <- parseFixedSize 4 jsonValue
        return $ V4 (xs !! 0) (xs !! 1) (xs !! 2) (xs !! 3)