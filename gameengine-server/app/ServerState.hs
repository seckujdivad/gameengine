module ServerState (ServerState (..), initialServerState, forEachClient) where

import Data.Map (Map, empty, update, toList, delete)
import Client (Client (Client))

data ServerState = ServerState {ssClients :: (Map Integer Client)}

-- |The ServerState when the server starts
initialServerState :: ServerState
initialServerState = ServerState empty

-- |Apply a function to each 'Client'. If 'Nothing' is returned, that 'Client' is removed (cleanup is not handled)
forEachClient :: ServerState -> (Client -> IO (Maybe Client)) -> IO (ServerState)
forEachClient serverState clientProcessor = do
    let
        clients = ssClients serverState

        clientProcessorWrapper :: (Integer, Client) -> IO (Integer, Maybe Client)
        clientProcessorWrapper (uid, client) = (clientProcessor client) >>= (\clientMaybe -> return (uid, clientMaybe))
        
    newClientsData <- listIOToIOList (fmap clientProcessorWrapper (toList clients))

    let
        clientFold :: (Integer, Maybe Client) -> Map Integer Client -> Map Integer Client
        clientFold (uid, clientMaybe) serverState = update (const clientMaybe) uid serverState

    return (serverState {ssClients = foldr clientFold clients newClientsData})

-- |Convert a list of 'IO' operations and their values to a single 'IO' operation with all their values preserved in a list
listIOToIOList :: [IO a] -> IO [a]
listIOToIOList = mconcat . map (fmap (\x -> [x]))