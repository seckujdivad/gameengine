module Client (Client (Client), getClientIdentifier, showClientMessage) where

import Network.Socket (Socket)

import TCPServer (ConnInfo (ConnInfo))


-- |All information stored about a client
data Client =
    -- |A normal client
    Client Socket ConnInfo (Maybe String)

-- |Get a human-readable identifier for a 'Client'. The first 'Bool' controls whether or not the UID is always included
getClientIdentifier :: Bool -> Client -> String
getClientIdentifier alwaysIncludeUID (Client _ (ConnInfo uid _) nameMaybe) = case nameMaybe of
    Just name -> name ++ if alwaysIncludeUID then " (" ++ show uid ++ ")" else ""
    Nothing -> show uid

-- |Show a 'String' in the console that is caused by a 'Client'
showClientMessage :: Client -> String -> String
showClientMessage client message = (getClientIdentifier True client) ++ " - " ++ message