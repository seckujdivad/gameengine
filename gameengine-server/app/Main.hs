module Main where

import Network.Socket (SockAddr, Socket, gracefulClose)
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

-- |Message sent from a receiver thread to the main thread
data ReceiverMsg =
    -- |Message sent from a receiver thread to the main thread
    ReceiverMsg ConnInfo ReceiverMsgInner
-- |Body of a 'ReceiverMsg'
data ReceiverMsgInner =
    -- |Sent when a new connection is made
    ClientConnEstablished Socket
    -- |Sent when a 'Packet' is received and successfully parsed
    | PackedReceived Packet
    -- |Sent when the connection is closed without error
    | ClientConnClosed
    -- |Sent when an error occurs in the receiver thread
    | ReceiverException String
    deriving (Show)

instance Show ReceiverMsg where
    show (ReceiverMsg (ConnInfo uid _) inner) = show uid ++ ": " ++ show inner

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
connHandler :: TChan ReceiverMsg -> Socket -> ConnInfo -> IO ()
connHandler mainloopIn connection connInfo = do
    putStrLn ("New connection: " ++ show connInfo)
    sendToTChan mainloopIn (ReceiverMsg connInfo (ClientConnEstablished connection))
    connReceiver mainloopIn connection connInfo
    where
        ConnInfo uid _ = connInfo

-- |Listens to a client
connReceiver :: TChan ReceiverMsg -> Socket -> ConnInfo -> IO ()
connReceiver mainloopIn connection connInfo = catch (do
    message <- recv connection 1024
    let socketClosed = Data.ByteString.null message
    if socketClosed then do
        putStrLn ("Client receiver closed - " ++ show connInfo)
        sendMessage $ ClientConnClosed
    else do
        sendMessage $ PackedReceived $ deserialise message --deserialise might throw an error, but sending malformed packets will only crash the receiver thread for that client
        connReceiver mainloopIn connection connInfo)
    (\exception -> do
        sendMessage $ ReceiverException (show (exception :: IOException)))
    where
        sendMessage = (sendToTChan mainloopIn) . (ReceiverMsg connInfo)

-- |Handles inter-client communication and the server state
serverMainloop :: TChan ReceiverMsg -> IO ()
serverMainloop mainloopIn = putStrLn "Awaiting connections..." >> serverMainloopInner mainloopIn empty >> putStrLn "Mainloop stopped"

serverMainloopInner :: TChan ReceiverMsg -> Map Integer Client -> IO ()
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

-- |Get a human-readable identifier for a 'Client'. The first 'Bool' controls whether or not the UID is always included
getClientIdentifier :: Bool -> Client -> String
getClientIdentifier alwaysIncludeUID (Client _ (ConnInfo uid _) nameMaybe) = case nameMaybe of
    Just name -> name ++ if alwaysIncludeUID then " (" ++ show uid ++ ")" else ""
    Nothing -> show uid

-- |Show a 'String' in the console that is caused by a 'Client'
showClientMessage :: Client -> String -> String
showClientMessage client message = (getClientIdentifier True client) ++ " - " ++ message

-- |Convert a list of 'IO' operations and their values to a single 'IO' operation with all their values preserved in a list
listIOToIOList :: [IO a] -> IO [a]
listIOToIOList = mconcat . map (fmap (\x -> [x]))