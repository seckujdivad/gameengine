module Mainloop (serverMainloop) where

import Control.Concurrent.STM.TChan (TChan)

import Data.Map (Map)
import Data.Map.Strict (insert, update, lookup)

import Network.Socket (Socket)

import AtomicTChan (readFromTChan)
import TCPServer (ConnInfo (ConnInfo))
import Packet (Packet (..))
import Client (Client (Client), getClientIdentifier, showClientMessage)
import MainloopMessage (MainloopMessage (..), ReceiverMsgInner (..))
import ConfigLoader (Config (..), CfgLevel (..))
import ServerState (ServerState (..), initialServerState)
import ClientApplicators (ClientApplicator, applyToAllClients, applyToClient)
import MainloopClientApplicators (closeClient, sendPacketToClient)


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
            Just newState <- applyToClient (sendPacketToClient (ConnEstablished uid)) uid (serverState {ssClients = insert uid (Client connection connInfo Nothing) (ssClients serverState)})
            nextLoop $ newState
        
        _ -> case Data.Map.Strict.lookup uid (ssClients serverState) of
            Just client -> case msgInner of
                ClientConnEstablished connection -> error "This case should be handled above - this line only exists to silence warnings"

                PackedReceived packet -> case Data.Map.Strict.lookup uid (ssClients serverState) of
                    Just client -> handlePacket mainloopIn config serverState client packet
                    Nothing -> putStrLn $ "Packet received for client with UID " ++ show uid ++ ", but this client doesn't exist"
                
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
    ConnEstablished _ -> do
        putStrLn $ showClientMessage client "client shouldn't send ConnEstablished"
        nextLoop serverState

    ClientChatMessage strMessage -> do
        newServerState <- applyToAllClients (sendPacketToClient (ServerChatMessage (getClientIdentifier False client) strMessage)) serverState
        nextLoop newServerState
    
    ServerChatMessage _ _ -> do
        putStrLn $ showClientMessage client "client shouldn't send ServerChatMessage"
        nextLoop serverState
    
    SetClientName newName -> do
        putStrLn $ showClientMessage client ("name set to " ++ newName)
        nextLoop $ serverState {ssClients = update (\(Client connection connInfo _) -> Just (Client connection connInfo (Just newName))) uid (ssClients serverState)}
    
    where
        nextLoop = serverMainloopInner mainloopIn config
        Client _ (ConnInfo uid _) _ = client