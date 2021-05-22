module Main where

import Network.Socket (SockAddr, Socket)
import Network.Socket.ByteString (recv, sendAll)

import Control.Monad (forever, unless, when)
import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan (TChan, newTChan, dupTChan, tryReadTChan, writeTChan, newBroadcastTChan, readTChan)

import qualified Data.ByteString (null)
import Data.ByteString.Char8 (pack, unpack)

import TCPServer (runTCPServer, ConnInfo (ConnInfo))
import Packet (Packet (ConnEstablished, ChatMessage), serialise, deserialise)

data ClientPacket = ClientPacket Integer (Maybe Packet) | NewClient ConnInfo Socket

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
        (ConnInfo uid address) = connInfo

-- |Listens to a client
connReceiver :: TChan ClientPacket -> Socket -> ConnInfo -> IO ()
connReceiver mainloopIn connection connInfo = do
    message <- recv connection 1024
    let socketClosed = Data.ByteString.null message
    if socketClosed then do
        putStrLn ("Client receiver closed - " ++ show connInfo)
        writeToInput $ ClientPacket uid Nothing
    else do
        case deserialise message of
            Nothing -> putStrLn "Couldn't decode packet"
            Just packet -> writeToInput $ ClientPacket uid (Just packet)
        connReceiver mainloopIn connection connInfo
    where
        (ConnInfo uid address) = connInfo
        writeToInput = sendToTChan mainloopIn

-- |Handles inter-client communication and the server state
serverMainloop :: TChan ClientPacket -> IO ()
serverMainloop mainloopIn = putStrLn "Awaiting connections..." >> serverMainloopInner mainloopIn []

serverMainloopInner :: TChan ClientPacket -> [(ConnInfo, Socket)] -> IO ()
serverMainloopInner mainloopIn clients = do
    clientComm <- readFromInput
    case clientComm of
        (ClientPacket uid packetMaybe) -> case packetMaybe of
            Just packet -> case packet of
                ConnEstablished uid -> putStrLn $ show uid ++ " - client shouldn't send ConnEstablished"
                ChatMessage strMessage -> do
                    putStrLn $ "Chat message - " ++ show uid ++ " - " ++ strMessage
                    foldl (>>) (return ()) [sendPacket connection packet | (connInfo, connection) <- clients]
                    nextLoop clients
            Nothing -> do
                putStrLn $ show uid ++ " - cleaning client"
                nextLoop $ filter (\(ConnInfo uid2 _, _) -> uid /= uid2) clients
        (NewClient (ConnInfo uid address) connection) -> do
            sendPacket connection (ConnEstablished uid)
            nextLoop $ clients ++ [(ConnInfo uid address, connection)]
    where
        readFromInput = readFromTChan mainloopIn
        nextLoop = serverMainloopInner mainloopIn

-- |Sends a 'Packet' to a 'Socket'
sendPacket :: Socket -> Packet -> IO ()
sendPacket socket = sendAll socket . serialise 

-- |Transactionally write some data to a 'TChan'
sendToTChan :: TChan a -> a -> IO ()
sendToTChan channel = atomically . writeTChan channel

-- |Transactionally read some data from a 'TChan'
readFromTChan :: TChan a -> IO a
readFromTChan = atomically . readTChan