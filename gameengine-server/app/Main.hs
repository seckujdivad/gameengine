module Main where

import Network.Socket (SockAddr, Socket, shutdown, ShutdownCmd (ShutdownBoth))
import Network.Socket.ByteString (recv, sendAll)

import Control.Monad (forever, unless, when)
import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan (TChan, newTChan)
import Control.Exception (catch, IOException)

import qualified Data.ByteString (null)
import Data.ByteString.Char8 (pack, unpack)
import Data.Map (Map)
import Data.Map.Strict (insert, update, lookup, empty, delete, keys)

import TCPServer (runTCPServer, ConnInfo (ConnInfo))
import AtomicTChan (sendToTChan, readFromTChan)

import Packet (Packet (ConnEstablished, ClientChatMessage, ServerChatMessage, SetClientName), serialise, deserialise)

data ClientPacket = ClientPacket Integer (Maybe Packet) | NewClient ConnInfo Socket

instance Show ClientPacket where
    show (ClientPacket uid packetMaybe) = show uid ++ ": " ++ maybe "no packet" show packetMaybe

-- |All information stored about a client
data Client =
    -- |A normal client
    Client Socket ConnInfo (Maybe String)

-- |Entry point
main :: IO ()
main = do
    mainloopIn <- atomically newTChan
    forkIO (serverMainloop mainloopIn)
    runTCPServer Nothing "4321" (connHandler mainloopIn)

-- |Called when a new connection to a client is established
connHandler :: TChan ClientPacket -> Socket -> ConnInfo -> IO ()
connHandler mainloopIn connection connInfo = do
    putStrLn ("New connection: " ++ show connInfo)
    sendToTChan mainloopIn (NewClient connInfo connection)
    connReceiver mainloopIn connection connInfo
    where
        (ConnInfo uid _) = connInfo

-- |Listens to a client
connReceiver :: TChan ClientPacket -> Socket -> ConnInfo -> IO ()
connReceiver mainloopIn connection connInfo = do
    message <- recv connection 1024
    let socketClosed = Data.ByteString.null message
    if socketClosed then do
        putStrLn ("Client receiver closed - " ++ show connInfo)
        writeToInput $ ClientPacket uid Nothing
    else do
        writeToInput $ ClientPacket uid $ Just $ deserialise message --deserialise might throw an error, but sending malformed packets will only crash the receiver thread for that client
        connReceiver mainloopIn connection connInfo
    where
        (ConnInfo uid address) = connInfo
        writeToInput = sendToTChan mainloopIn

-- |Handles inter-client communication and the server state
serverMainloop :: TChan ClientPacket -> IO ()
serverMainloop mainloopIn = putStrLn "Awaiting connections..." >> serverMainloopInner mainloopIn empty >> putStrLn "Mainloop stopped"

serverMainloopInner :: TChan ClientPacket -> Map Integer Client -> IO ()
serverMainloopInner mainloopIn clients = do
    clientComm <- readFromInput
    case clientComm of
        (ClientPacket uid packetMaybe) -> case Data.Map.Strict.lookup uid clients of
            Just client -> do
                let (Client _ connInfo clientNameMaybe) = client
                case packetMaybe of
                    Just packet -> case packet of
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
                                
                                sendResults :: [IO (Maybe (Integer, String))]
                                sendResults = map clientProcessor (keys clients)

                                clientErrors :: IO [Maybe (Integer, String)]
                                clientErrors = foldl mappend (return []) (map (\operation -> operation >>= (\errorDescriptionMaybe -> return [errorDescriptionMaybe])) sendResults)
                            
                            errorMaybes <- clientErrors
                            let filteredErrorMaybes = filter (maybe False (const True)) errorMaybes
                            let errors = map (\(Just error) -> error) filteredErrorMaybes

                            mconcat (map (\(uid, errorText) -> do
                                    let clientMaybe = Data.Map.Strict.lookup uid clients
                                    case clientMaybe of
                                        Just client -> do
                                            let Client connection _ _ = client
                                            putStrLn (showClientMessage client "dropping connection - error while sending to client: " ++ errorText)
                                            catch (shutdown connection ShutdownBoth) (const (return ()) :: IOException -> IO ()) --throws if the connection isn't active
                                        Nothing -> return ())
                                errors)

                            let
                                clientFilterer :: Map Integer Client -> [(Integer, String)] -> Map Integer Client
                                clientFilterer clients ((uid, error):otherErrors) = delete uid clients
                                clientFilterer clients [] = clients

                            nextLoop (clientFilterer clients errors)
                        
                        ServerChatMessage _ _ -> do
                            putStrLn $ showClientMessage client "client shouldn't send ServerChatMessage"
                            nextLoop clients
                        
                        SetClientName newName -> do
                            putStrLn $ showClientMessage client ("name set to " ++ newName)
                            nextLoop (update (\(Client connection connInfo _) -> Just (Client connection connInfo (Just newName))) uid clients)

                    Nothing -> do
                        putStrLn $ showClientMessage client "cleaning client"
                        nextLoop $ delete uid clients
            
            Nothing -> putStrLn $ "Client UID " ++ show uid ++ " does not exist"

        (NewClient (ConnInfo uid address) connection) -> do
            sendPacket connection (ConnEstablished uid)
            nextLoop $ insert uid (Client connection (ConnInfo uid address) Nothing) clients

    where
        readFromInput = readFromTChan mainloopIn
        nextLoop = serverMainloopInner mainloopIn

-- |Sends a 'Packet' to a 'Socket'. Returns 'True' if the 'Packet' was sent without error, 'False' otherwise
sendPacket :: Socket -> Packet -> IO (Maybe String)
sendPacket socket packet = catch (do
        sendPacketInner socket packet
        return Nothing) (\error -> return (Just (show (error :: IOException))))

-- |Sends a 'Packet' to a 'Socket'
sendPacketInner :: Socket -> Packet -> IO ()
sendPacketInner socket = sendAll socket . serialise

-- |Get a 
getClientIdentifier :: Bool -> Client -> String
getClientIdentifier alwaysIncludeUID (Client _ (ConnInfo uid _) nameMaybe) = case nameMaybe of
    Just name -> name ++ if alwaysIncludeUID then " (" ++ show uid ++ ")" else ""
    Nothing -> show uid

showClientMessage :: Client -> String -> String
showClientMessage client message = (getClientIdentifier True client) ++ " - " ++ message