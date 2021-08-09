module GameEngineServer.Mainloop.Mainloop (serverMainloop) where

import Control.Concurrent.STM.TChan (TChan)

import Data.Map.Strict (insert, update, lookup)

import GameEngineServer.AtomicTChan (readFromTChan)
import GameEngineServer.Network.TCPServer (ConnInfo (ConnInfo))
import GameEngineServer.Network.Packet (Packet (..), packetToPacketType)
import GameEngineServer.Mainloop.MainloopMessage (MainloopMessage (..), ReceiverMsgInner (..))
import GameEngineServer.Mainloop.MainloopClientApplicators (closeClient, sendPacketToClient)
import GameEngineServer.Config.Config (Config (..), CfgLevel (..))
import GameEngineServer.State.ServerState (ServerState (..), initialServerState)
import GameEngineServer.State.ClientApplicators (applyToAllClients, applyToClient)
import GameEngineServer.State.Client (Client (Client), getClientIdentifier, showClientMessage)


-- |Handles inter-client communication and the server state
serverMainloop :: TChan MainloopMessage -> Config -> IO ()
serverMainloop mainloopIn config = do
    putStrLn "Awaiting connections..."
    serverMainloopInner mainloopIn config (initialServerState config)
    putStrLn "Mainloop stopped"

serverMainloopInner :: TChan MainloopMessage -> Config -> ServerState -> IO ()
serverMainloopInner mainloopIn config serverState = do
    ReceiverMsg connInfo msgInner <- readFromTChan mainloopIn
    let ConnInfo uid _ = connInfo
    case msgInner of
        ClientConnEstablished connection -> do
            let clientActions = sendPacketToClient (ConnEstablished uid) <> sendPacketToClient (SetScene (cfglvlRoot $ ssCurrentLevel serverState) (cfglvlFile $ ssCurrentLevel serverState))
            Just newState <- applyToClient clientActions uid (serverState {ssClients = insert uid (Client connection connInfo Nothing) (ssClients serverState)})
            nextLoop $ newState
        
        _ -> case Data.Map.Strict.lookup uid (ssClients serverState) of
            Just client -> case msgInner of
                ClientConnEstablished _ -> error "This case should be handled above - this line only exists to silence warnings"

                PackedReceived packet -> handlePacket mainloopIn config serverState client packet
                
                ReceiverException errorMsg -> do
                    putStrLn $ showClientMessage client "listener threw an error - " ++ errorMsg
                    Just newServerState <- applyToClient closeClient uid serverState
                    nextLoop newServerState

                ClientConnClosed -> do
                    Just newServerState <- applyToClient closeClient uid serverState
                    nextLoop newServerState

            Nothing -> do
                putStrLn $ "Client UID " ++ show uid ++ " does not exist"
                nextLoop serverState

    where
        nextLoop = serverMainloopInner mainloopIn config

handlePacket :: TChan MainloopMessage -> Config -> ServerState -> Client -> Packet -> IO ()
handlePacket mainloopIn config serverState client packet = case packet of
    ClientChatMessage strMessage -> do
        newServerState <- applyToAllClients (sendPacketToClient (ServerChatMessage (getClientIdentifier False client) strMessage)) serverState
        nextLoop newServerState
    
    SetClientName newName -> do
        putStrLn $ showClientMessage client ("name set to " ++ newName)
        nextLoop $ serverState {ssClients = update (\(Client connection connInfo _) -> Just (Client connection connInfo (Just newName))) uid (ssClients serverState)}
    
    _ -> do
        putStrLn $ showClientMessage client $ "client shouldn't send packet of type " ++ (show $ packetToPacketType packet)
        nextLoop serverState
    
    where
        nextLoop = serverMainloopInner mainloopIn config
        Client _ (ConnInfo uid _) _ = client