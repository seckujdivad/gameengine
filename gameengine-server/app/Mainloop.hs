module Mainloop (serverMainloop) where

import Control.Concurrent.STM.TChan (TChan)
import Control.Exception (catch, IOException)

import Data.Map (Map)
import Data.Map.Strict (insert, update, lookup, empty, delete, keys)

import Network.Socket (Socket, gracefulClose)
import Network.Socket.ByteString (sendAll)

import AtomicTChan (readFromTChan)
import TCPServer (ConnInfo (ConnInfo))
import Packet (Packet (ConnEstablished, ClientChatMessage, ServerChatMessage, SetClientName), serialise)
import Client (Client (Client), getClientIdentifier, showClientMessage)
import MainloopMessage (MainloopMessage (ReceiverMsg), ReceiverMsgInner (ClientConnEstablished, PackedReceived, ClientConnClosed, ReceiverException))

-- |Handles inter-client communication and the server state
serverMainloop :: TChan MainloopMessage -> IO ()
serverMainloop mainloopIn = do
    putStrLn "Awaiting connections..."
    serverMainloopInner mainloopIn empty
    putStrLn "Mainloop stopped"

serverMainloopInner :: TChan MainloopMessage -> Map Integer Client -> IO ()
serverMainloopInner mainloopIn clients = do
    ReceiverMsg connInfo msgInner <- readFromTChan mainloopIn
    let ConnInfo uid address = connInfo
    case msgInner of
        ClientConnEstablished connection -> do
            sendPacket connection (ConnEstablished uid)
            nextLoop $ insert uid (Client connection (ConnInfo uid address) Nothing) clients
        
        _ ->  case Data.Map.Strict.lookup uid clients of
            Just client -> case msgInner of
                ClientConnEstablished connection -> do
                    sendPacket connection (ConnEstablished uid)
                    nextLoop $ insert uid (Client connection (ConnInfo uid address) Nothing) clients

                PackedReceived packet -> case Data.Map.Strict.lookup uid clients of
                    Just client -> case packet of
                        ConnEstablished _ -> do
                            putStrLn $ showClientMessage client "client shouldn't send ConnEstablished"
                            nextLoop clients

                        ClientChatMessage strMessage -> do
                            putStrLn $ showClientMessage client ("Chat message - " ++ strMessage)

                            let
                                clientProcessor :: Integer -> IO (Maybe (Integer, String))
                                clientProcessor key = case Data.Map.Strict.lookup key clients of
                                    Just (Client connection _ _) -> do
                                        errMsgMaybe <- sendPacket connection (ServerChatMessage (getClientIdentifier False client) strMessage)
                                        case errMsgMaybe of
                                            Just errMsg -> return (Just (key, errMsg))
                                            Nothing -> return Nothing
                                    Nothing -> return Nothing
                            
                            errorMaybes <- listIOToIOList (map clientProcessor (keys clients))

                            let
                                errors = concat (fmap (\errorMaybe -> case errorMaybe of
                                    Just error -> [error]
                                    Nothing -> []) errorMaybes)

                                connectionDropper :: (Integer, String) -> IO ()
                                connectionDropper (uid, errorText) = case Data.Map.Strict.lookup uid clients of
                                    Just client -> do
                                        let Client connection _ _ = client
                                        putStrLn (showClientMessage client "dropping connection - error while sending to client: " ++ errorText)
                                        catch (gracefulClose connection 5000) (const (return ()) :: IOException -> IO ()) --throws if the connection isn't active
                                    Nothing -> return ()

                            mconcat (map connectionDropper errors)
                            nextLoop (foldr (\(uid, _) clients -> delete uid clients) clients errors)
                        
                        ServerChatMessage _ _ -> do
                            putStrLn $ showClientMessage client "client shouldn't send ServerChatMessage"
                            nextLoop clients
                        
                        SetClientName newName -> do
                            putStrLn $ showClientMessage client ("name set to " ++ newName)
                            nextLoop (update (\(Client connection connInfo _) -> Just (Client connection connInfo (Just newName))) uid clients)
                
                ReceiverException errorMsg -> do
                    putStrLn $ showClientMessage client "listener threw an error - " ++ errorMsg
                    newClients <- closeClient client
                    nextLoop newClients

                ClientConnClosed -> do
                    newClients <- closeClient client
                    nextLoop newClients

            Nothing -> putStrLn $ "Client UID " ++ show uid ++ " does not exist"

    where
        nextLoop = serverMainloopInner mainloopIn

        -- |Close the given 'Client' and give the leftover clients
        closeClient :: Client -> IO (Map Integer Client)
        closeClient client = do
            putStrLn $ showClientMessage client "closing client"
            let Client connection (ConnInfo uid _) _ = client
            catch (gracefulClose connection 5000) (const (return ()) :: IOException -> IO ()) --throws if the connection isn't active
            return (delete uid clients)

-- |Sends a 'Packet' to a 'Socket'. Returns 'True' if the 'Packet' was sent without error, 'False' otherwise
sendPacket :: Socket -> Packet -> IO (Maybe String)
sendPacket socket packet = catch (do
        sendPacketInner socket packet
        return Nothing) (\error -> return (Just (show (error :: IOException))))

-- |Sends a 'Packet' to a 'Socket'
sendPacketInner :: Socket -> Packet -> IO ()
sendPacketInner socket = sendAll socket . serialise

-- |Convert a list of 'IO' operations and their values to a single 'IO' operation with all their values preserved in a list
listIOToIOList :: [IO a] -> IO [a]
listIOToIOList = mconcat . map (fmap (\x -> [x]))