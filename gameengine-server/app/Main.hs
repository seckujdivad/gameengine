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

import qualified RecvToMain as RToM
import qualified MainToSend as MToS


type ServerInterface = (TChan RToM.RecvToMain, TChan MToS.MainToSend) --mainloop input, mainloop output

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
        writeToInput (RToM.RecvToMain uid RToM.Close)
    else do
        case deserialise message of
            Nothing -> putStrLn "Couldn't decode packet"
            Just packet -> case packet of
                ConnEstablished uid -> putStrLn (show connInfo ++ " - client shouldn't send ConnEstablished")
                ChatMessage strMessage -> do
                    putStrLn ("Chat message - " ++ show connInfo ++ " - " ++ strMessage)
                    writeToInput (RToM.RecvToMain uid (RToM.Message strMessage))
        connReceiver (mainloopIn, mainloopOut) connection connInfo
    where
        (ConnInfo uid address) = connInfo
        writeToInput = sendToTChan mainloopIn

connSender :: Socket -> ConnInfo -> TChan MToS.MainToSend -> IO ()
connSender connection connInfo mainloopOut = do
    nextMessage <- atomically $ readTChan mainloopOut
    case nextMessage of
        MToS.Message strMessage -> do
            putStrLn ("Rebroadcasting to " ++ show connInfo ++ " - " ++ strMessage)
            sendPacket connection (ChatMessage strMessage)
            connSender connection connInfo mainloopOut
        MToS.Close uidToClose -> unless (uid == uidToClose) (connSender connection connInfo mainloopOut)
    where
        (ConnInfo uid address) = connInfo

serverMainloop :: ServerInterface -> IO ()
serverMainloop (mainloopIn, mainloopOut) = forever $ do
    (RToM.RecvToMain uid message) <- readFromInput
    
    case message of
        RToM.Message strMessage -> writeToOutput (MToS.Message strMessage)
        RToM.Close -> writeToOutput (MToS.Close uid)
    
    where
        writeToOutput = sendToTChan mainloopOut
        readFromInput = readFromTChan mainloopIn

sendPacket :: Socket -> Packet -> IO ()
sendPacket socket = sendAll socket . serialise 

sendToTChan :: TChan a -> a -> IO ()
sendToTChan channel = atomically . writeTChan channel

readFromTChan :: TChan a -> IO a
readFromTChan = atomically . readTChan