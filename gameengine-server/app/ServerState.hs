module ServerState where

import Data.Map (Map, empty)
import Client (Client)

data ServerState = ServerState {ssClients :: (Map Integer Client)}

initialServerState :: ServerState
initialServerState = ServerState empty