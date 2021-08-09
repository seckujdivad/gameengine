{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.VectorLoader (JSONableV1 (..), JSONableV2 (..), JSONableV3 (..), JSONableV4 (..), extractJSONableV1, extractJSONableV2, extractJSONableV3, extractJSONableV4) where

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

-- |Wraps a 'V1' so that a 'FromJSON' instance can be created without it being an orphan
newtype JSONableV1 a =
    -- |Wraps a 'V1' so that a 'FromJSON' instance can be created without it being an orphan
    JSONableV1 (V1 a)

-- |Extract the inner 'V1' from a 'JSONableV1'
extractJSONableV1 :: JSONableV1 a -> V1 a
extractJSONableV1 (JSONableV1 result) = result

instance FromJSON a => FromJSON (JSONableV1 a) where
    parseJSON jsonValue = do
        [x] <- parseFixedSize 1 jsonValue
        return $ JSONableV1 $ V1 x

-- |Wraps a 'V2' so that a 'FromJSON' instance can be created without it being an orphan
newtype JSONableV2 a =
    -- |Wraps a 'V2' so that a 'FromJSON' instance can be created without it being an orphan
    JSONableV2 (V2 a)

-- |Extract the inner 'V2' from a 'JSONableV2'
extractJSONableV2 :: JSONableV2 a -> V2 a
extractJSONableV2 (JSONableV2 result) = result

instance FromJSON a => FromJSON (JSONableV2 a) where
    parseJSON jsonValue = do
        xs <- parseFixedSize 2 jsonValue
        return $ JSONableV2 $ V2 (xs !! 0) (xs !! 1)

-- |Wraps a 'V3' so that a 'FromJSON' instance can be created without it being an orphan
newtype JSONableV3 a =
    -- |Wraps a 'V3' so that a 'FromJSON' instance can be created without it being an orphan
    JSONableV3 (V3 a)

-- |Extract the inner 'V3' from a 'JSONableV3'
extractJSONableV3 :: JSONableV3 a -> V3 a
extractJSONableV3 (JSONableV3 result) = result

instance FromJSON a => FromJSON (JSONableV3 a) where
    parseJSON jsonValue = do
        xs <- parseFixedSize 3 jsonValue
        return $ JSONableV3 $ V3 (xs !! 0) (xs !! 1) (xs !! 2)

-- |Wraps a 'V4' so that a 'FromJSON' instance can be created without it being an orphan
newtype JSONableV4 a =
    -- |Wraps a 'V4' so that a 'FromJSON' instance can be created without it being an orphan
    JSONableV4 (V4 a)

-- |Extract the inner 'V4' from a 'JSONableV4'
extractJSONableV4 :: JSONableV4 a -> V4 a
extractJSONableV4 (JSONableV4 result) = result

instance FromJSON a => FromJSON (JSONableV4 a) where
    parseJSON jsonValue = do
        xs <- parseFixedSize 4 jsonValue
        return $ JSONableV4 $ V4 (xs !! 0) (xs !! 1) (xs !! 2) (xs !! 3)