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


data ServerMessage = Message String | Close Integer;

type ServerInterface = (TChan ServerMessage, TChan ServerMessage) --mainloop input, mainloop output

main :: IO ()
main = do
    mainloopIn <- atomically newTChan
    mainloopOut <- atomically newBroadcastTChan
    let interface = (mainloopIn, mainloopOut)
    forkIO (serverMainloop interface)
    runTCPServer Nothing "4321" (connHandler interface)

connHandler :: ServerInterface -> Socket -> ConnInfo -> IO ()
connHandler interfaceBlock connection connInfo = do
    putStrLn ("New connection: " ++ show connInfo)
    sendPacket connection (ConnEstablished uid)
    connReceiver interfaceBlock connection connInfo
    where
        (ConnInfo uid address) = connInfo

connReceiver :: ServerInterface -> Socket -> ConnInfo -> IO ()
connReceiver (mainloopIn, mainloopOut) connection connInfo = do
    message <- recv connection 1024
    let socketClosed = Data.ByteString.null message
    if socketClosed then do
        putStrLn ("Client receiver closed - " ++ show connInfo)
        atomically $ writeTChan mainloopIn (Close uid)
    else do
        let strMessage = unpack message
        putStrLn ("Message received - " ++ strMessage)
        atomically $ writeTChan mainloopIn (Message strMessage)
        connReceiver (mainloopIn, mainloopOut) connection connInfo
    where
        (ConnInfo uid address) = connInfo

connSender :: Socket -> ConnInfo -> TChan ServerMessage -> IO ()
connSender connection connInfo mainloopOut = do
    nextMessage <- atomically $ readTChan mainloopOut
    case nextMessage of
        Message strMessage -> do
            putStrLn ("Rebroadcasting to " ++ show connInfo ++ " - " ++ strMessage)
            sendPacket connection (ChatMessage strMessage)
            connSender connection connInfo mainloopOut
        Close uidToClose -> do
            unless (uid == uidToClose) (connSender connection connInfo mainloopOut)
    where
        (ConnInfo uid address) = connInfo

serverMainloop :: ServerInterface -> IO ()
serverMainloop (mainloopIn, mainloopOut) = forever $ do
    message <- atomically $ readTChan mainloopIn
    atomically $ writeTChan mainloopOut message

sendPacket :: Socket -> Packet -> IO ()
sendPacket socket = sendAll socket . serialise 