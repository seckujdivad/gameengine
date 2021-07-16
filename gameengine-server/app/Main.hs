module Main where

import Network.Socket (Socket)
import Network.Socket.ByteString (recv)

import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan (TChan, newTChan)
import Control.Exception (catch, IOException)

import qualified Data.ByteString (null)

import System.Environment (getArgs)
import System.Directory (setCurrentDirectory)

import TCPServer (runTCPServer, ConnInfo (..))
import AtomicTChan (sendToTChan)
import Mainloop (serverMainloop)
import MainloopMessage (MainloopMessage (ReceiverMsg), ReceiverMsgInner (..))
import Packet (deserialise)
import ConfigLoader (loadConfig)
import FlagProcessing (Flag (..), getFlags, getFlagUsageInfo)


-- |Entry point
main :: IO ()
main = do
    rawArgs <- getArgs

    let
        flagProcessor :: Flag -> IO ()
        flagProcessor flag = case flag of
            Help -> putStrLn getFlagUsageInfo
            WorkingDirectory newDirectory -> setCurrentDirectory newDirectory

    case getFlags rawArgs of
        Left flags -> mconcat $ fmap flagProcessor flags
        Right message -> error ("\n" ++ message)

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
connReceiver mainloopIn connection connInfo = catch (
        do
            message <- recv connection 1024
            let socketClosed = Data.ByteString.null message
            if socketClosed then do
                putStrLn $ show connInfo ++ " - connection was interrupted"
                sendMessage ClientConnClosed
            else do
                sendMessage $ PackedReceived $ deserialise message --deserialise might throw an error, but sending malformed packets will only crash the receiver thread for that client
                connReceiver mainloopIn connection connInfo
        )
    (\exception -> sendMessage $ ReceiverException (show (exception :: IOException)))
    where
        sendMessage :: ReceiverMsgInner -> IO ()
        sendMessage message = sendToTChan mainloopIn (ReceiverMsg connInfo message)