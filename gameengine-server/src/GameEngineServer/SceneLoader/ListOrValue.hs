module GameEngineServer.SceneLoader.ListOrValue (ListOrValue (..), listOrValueToList) where

import Data.Aeson (FromJSON, parseJSON, Value (Array))

import Data.Vector (toList)


-- |Stores either a list or a single value
data ListOrValue a =
    -- |Stores a list of values
    List [a]
    -- |Stores a single value
    | Value a

instance FromJSON a => FromJSON (ListOrValue a) where
    parseJSON (Array arr) = do
        values <- mapM parseJSON (toList arr)
        return $ List values
    parseJSON value = fmap Value (parseJSON value)

-- |Turns a 'ListOrValue' into a list containing either all the list elements or the single value on its own
listOrValueToList :: ListOrValue a -> [a]
listOrValueToList (List values) = values
listOrValueToList (Value a) = [a]