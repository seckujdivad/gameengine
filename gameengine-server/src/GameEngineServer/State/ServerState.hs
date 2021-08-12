module GameEngineServer.State.ServerState (ServerState (..), initialServerState, applyToAllClients, applyToClient) where

{-
Describes the current state of the server and provides utility methods for operating on that state.

The applyTo[All]Client[s] methods provide an abstracted way to process 'Client's. They exist
because both filtering and carrying out IO operations at the same time on a number of 'Client's
is cumbersome. The functions in 'ClientApplicators.hs' provide a better interface that recognises
that common results (like exceptions) should be handled by the applicator, not on a
function-by-function basis.
-}

import Data.Map (Map, empty, update, lookup, keys)

import GameEngineServer.State.Client (Client)
import GameEngineServer.Config.Config (Config (..), CfgLevel (..))
import GameEngineServer.State.Scene.Scene (Scene (..))


-- |Describes the current state of the server
data ServerState =
    -- |Describes the current state of the server
    ServerState {
        -- |'Client's that are connected to the server
        ssClients :: Map Integer Client,
        ssCurrentLevel :: CfgLevel,
        ssScene :: Scene
    }

-- |The 'ServerState' when the server starts
initialServerState :: Config -> ServerState
initialServerState config = ServerState empty (cfgInitialScene config) (Scene [])

-- |Apply a function to each 'Client'. If 'Nothing' is returned, that 'Client' is removed (cleanup is not handled)
applyToAllClients :: (Client -> IO (Maybe Client)) -> ServerState -> IO ServerState
applyToAllClients clientProcessor serverState = foldl clientFolder (return serverState) (keys (ssClients serverState))
    where
        clientFolder :: IO ServerState -> Integer -> IO ServerState
        clientFolder serverStateIO uid = do
            newServerState <- serverStateIO
            resultingServerStateMaybe <- applyToClient clientProcessor uid newServerState
            case resultingServerStateMaybe of
                Just resultingServerState -> return resultingServerState
                Nothing -> error "This UID should exist"

-- |Apply a function to a 'Client', removing it if 'Nothing' is returned and returning 'Nothing' if the UID doesn't exist
applyToClient :: (Client -> IO (Maybe Client)) -> Integer -> ServerState -> IO (Maybe ServerState)
applyToClient clientProcessor uid serverState = case Data.Map.lookup uid (ssClients serverState) of
    Just client -> do
        processedClientMaybe <- clientProcessor client
        return $ Just $ serverState {ssClients = update (const processedClientMaybe) uid (ssClients serverState)}
    Nothing -> return Nothing