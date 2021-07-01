module ClientApplicators (ClientApplicator, applyToAllClients, applyToClient) where

{-
Functions for applying an operation onto a 'Client'. They wrap the
functions in ServerState.hs with cleanup and error message
functionality.
-}

import Client (Client (Client), showClientMessage)
import SocketOperations (closeConnection)
import qualified ServerState


{-|
Type of function to be wrapped by 'clientApplicatorWrapper'

When applied to a 'Client' in a 'ServerState', the result will have the following effect:
- If Left Just 'Client' is returned, the processed 'Client' will be replaced with the returned 'Client'.
- If Left Nothing is returned, the processed 'Client' is removed with a simple message. The implication is that this is a normal, expected code path.
- If Right 'String' is returned, the provided 'String' is outputted in the context of being an error message and the processed 'Client' is removed.

When either result causes the 'Client' to be removed, the 'Socket' associated with that client is also closed.
-}
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