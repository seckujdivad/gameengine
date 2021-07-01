module ClientApplicators (ClientApplicator, applyToAllClients, applyToClient) where

{-
Functions for applying an operation onto a 'Client'. They wrap the
functions in ServerState.hs with cleanup and error message
functionality.
-}

import Client (Client (Client), showClientMessage)
import SocketOperations (closeConnection)
import qualified ServerState

-- |Type of function to be wrapped by 'clientApplicatorWrapper'
type ClientApplicator = Client -> IO (Either (Maybe Client) String)

-- |Implements 'ServerState.applyToAllClients', but wraps the 'Client' processor in 'clientApplicatorWrapper'
applyToAllClients :: ClientApplicator -> ServerState.ServerState -> IO ServerState.ServerState
applyToAllClients = ServerState.applyToAllClients . clientApplicatorWrapper

-- |Implements 'ServerState.applyToClient', but wraps the 'Client' processor in 'clientApplicatorWrapper'
applyToClient :: ClientApplicator -> Integer -> ServerState.ServerState -> IO (Maybe ServerState.ServerState)
applyToClient = ServerState.applyToClient . clientApplicatorWrapper

-- |Wraps a function that is applied to a 'Client' that may throw an error (in the form of returning a 'String')
clientApplicatorWrapper :: ClientApplicator -> Client -> IO (Maybe Client)
clientApplicatorWrapper clientApplicator client = do
    let Client connection _ _ = client
    clientMaybeOrError <- clientApplicator client
    case clientMaybeOrError of
        Left clientMaybe -> case clientMaybe of
            Just client -> return $ Just client
            Nothing -> do
                putStrLn (showClientMessage client "dropping connection")
                closeConnection connection
                return Nothing
        Right errorMsg -> do
            putStrLn (showClientMessage client "dropping connection - error while sending to client: " ++ errorMsg)
            closeConnection connection
            return Nothing