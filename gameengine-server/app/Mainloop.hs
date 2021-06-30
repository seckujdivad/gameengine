module Mainloop (serverMainloop) where

import Control.Concurrent.STM.TChan (TChan)
import Control.Exception (catch, IOException)

import Data.Map (Map)
import Data.Map.Strict (insert, update, lookup, empty, delete, keys)

import Network.Socket (Socket, gracefulClose)
import Network.Socket.ByteString (sendAll)

import AtomicTChan (readFromTChan)
import TCPServer (ConnInfo (ConnInfo))
import Packet (Packet (..), serialise)
import Client (Client (Client), getClientIdentifier, showClientMessage)
import MainloopMessage (MainloopMessage (..), ReceiverMsgInner (..))
import ConfigLoader (Config (..), CfgLevel (..))
import ServerState (ServerState (..), initialServerState, forEachClient)

-- |Sends a 'Packet' to a 'Socket'. Returns 'True' if the 'Packet' was sent without error, 'False' otherwise
sendPacket :: Socket -> Packet -> IO (Maybe String)
sendPacket socket packet = catch (do
        sendPacketInner socket packet
        return Nothing) (\error -> return (Just (show (error :: IOException))))

-- |Sends a 'Packet' to a 'Socket'
sendPacketInner :: Socket -> Packet -> IO ()
sendPacketInner socket = sendAll socket . serialise

-- |Handles inter-client communication and the server state
serverMainloop :: TChan MainloopMessage -> Config -> IO ()
serverMainloop mainloopIn config = do
    putStrLn "Awaiting connections..."
    serverMainloopInner mainloopIn config initialServerState
    putStrLn "Mainloop stopped"

serverMainloopInner :: TChan MainloopMessage -> Config -> ServerState -> IO ()
serverMainloopInner mainloopIn config serverState = do
    ReceiverMsg connInfo msgInner <- readFromTChan mainloopIn
    let ConnInfo uid _ = connInfo
    case msgInner of
        ClientConnEstablished connection -> do
            sendPacket connection (ConnEstablished uid)
            nextLoop $ serverState {ssClients = insert uid (Client connection connInfo Nothing) (ssClients serverState)}
        
        _ ->  case Data.Map.Strict.lookup uid (ssClients serverState) of
            Just client -> case msgInner of
                ClientConnEstablished connection -> do
                    sendPacket connection (ConnEstablished uid)
                    nextLoop $ serverState {ssClients = insert uid (Client connection connInfo Nothing) (ssClients serverState)}

                PackedReceived packet -> case Data.Map.Strict.lookup uid (ssClients serverState) of
                    Just client -> handlePacket mainloopIn config serverState client packet
                    
                    Nothing -> do
                        putStrLn $ "Packet received for client with UID " ++ show uid ++ ", but this client doesn't exist"
                
                ReceiverException errorMsg -> do
                    putStrLn $ showClientMessage client "listener threw an error - " ++ errorMsg
                    newClients <- closeClient client
                    nextLoop newClients

                ClientConnClosed -> do
                    newClients <- closeClient client
                    nextLoop newClients

            Nothing -> do
                putStrLn $ "Client UID " ++ show uid ++ " does not exist"
                nextLoop serverState

    where
        nextLoop = serverMainloopInner mainloopIn config

        -- |Close the given 'Client' and give the resulting server state
        closeClient :: Client -> IO (ServerState)
        closeClient client = do
            putStrLn $ showClientMessage client "closing client"
            let Client connection (ConnInfo uid _) _ = client
            catch (gracefulClose connection 5000) (const (return ()) :: IOException -> IO ()) --throws if the connection isn't active
            return $ serverState {ssClients = delete uid (ssClients serverState)}

handlePacket :: TChan MainloopMessage -> Config -> ServerState -> Client -> Packet -> IO ()
handlePacket mainloopIn config serverState client packet = case packet of
    ConnEstablished _ -> do
        putStrLn $ showClientMessage client "client shouldn't send ConnEstablished"
        nextLoop serverState

    ClientChatMessage strMessage -> do
        let
            clientProcessor :: Client -> IO (Maybe Client)
            clientProcessor client = do
                let Client connection _ _ = client
                clientOrError <- clientProcessorInner client
                case clientOrError of
                    Left client -> return $ Just client
                    Right errorMsg -> do
                        putStrLn (showClientMessage client "dropping connection - error while sending to client: " ++ errorMsg)
                        catch (gracefulClose connection 5000) (const (return ()) :: IOException -> IO ()) --throws if the connection isn't active
                        return Nothing

            clientProcessorInner :: Client -> IO (Either Client String)
            clientProcessorInner client = do
                let Client connection _ _ = client
                errorMsgMaybe <- sendPacket connection (ServerChatMessage (getClientIdentifier False client) strMessage)
                case errorMsgMaybe of
                    Just errorMsg -> return $ Right errorMsg
                    Nothing -> return $ Left client
        
        newServerState <- forEachClient serverState clientProcessor
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