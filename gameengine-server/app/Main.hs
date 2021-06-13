module Main where

import Network.Socket (Socket)
import Network.Socket.ByteString (recv)

import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan (TChan, newTChan)
import Control.Exception (catch, IOException)

import qualified Data.ByteString (null)

import TCPServer (runTCPServer, ConnInfo (..))
import AtomicTChan (sendToTChan)
import Mainloop (serverMainloop)
import MainloopMessage (MainloopMessage (ReceiverMsg), ReceiverMsgInner (..))
import Packet (deserialise)
import ConfigLoader (loadConfig)

-- |Entry point
main :: IO ()
main = do
    configMaybe <- loadConfig
    case configMaybe of
        Just config -> do
            mainloopIn <- atomically newTChan
            forkIO (serverMainloop mainloopIn config)
            runTCPServer Nothing "4321" (connHandler mainloopIn)

        Nothing -> putStrLn "Couldn't load config file"

-- |Called when a new connection to a client is established
connHandler :: TChan MainloopMessage -> Socket -> ConnInfo -> IO ()
connHandler mainloopIn connection connInfo = do
    putStrLn ("New connection: " ++ show connInfo)
    sendToTChan mainloopIn (ReceiverMsg connInfo (ClientConnEstablished connection))
    connReceiver mainloopIn connection connInfo
    where
        ConnInfo uid _ = connInfo

-- |Listens to a client
connReceiver :: TChan MainloopMessage -> Socket -> ConnInfo -> IO ()
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