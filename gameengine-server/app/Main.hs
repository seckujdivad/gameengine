module Main where

import Network.Socket (SockAddr, Socket)
import Network.Socket.ByteString (recv, sendAll)

import Control.Monad (forever, unless, when)
import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan (TChan, newTChan, dupTChan, tryReadTChan, writeTChan, newBroadcastTChan, readTChan)

import qualified Data.ByteString (null)
import Data.ByteString.Char8 (pack, unpack)

import TCPServer (runTCPServer)

type ServerInterface = (TChan String, TChan String) --mainloop input, mainloop output

main :: IO ()
main = do
    mainloopIn <- atomically newTChan
    mainloopOut <- atomically newBroadcastTChan
    let interface = (mainloopIn, mainloopOut)
    forkIO (serverMainloop interface)
    runTCPServer Nothing "4321" (connHandler interface)

connHandler :: ServerInterface -> Socket -> SockAddr -> IO ()
connHandler (mainloopIn, mainloopOut) connection address = do
    putStrLn ("New connection: " ++ show address)
    mainloopOutDuplicated <- atomically $ dupTChan mainloopOut
    forkIO (connSender connection address mainloopOutDuplicated)
    handleMessageFromClient (mainloopIn, mainloopOut) connection

handleMessageFromClient :: ServerInterface -> Socket -> IO ()
handleMessageFromClient (mainloopIn, mainloopOut) connection = do
    message <- recv connection 1024
    let socketClosed = Data.ByteString.null message
    when socketClosed (putStrLn "Connection closed")
    unless socketClosed (do
        let strMessage = unpack message
        atomically $ writeTChan mainloopIn strMessage
        handleMessageFromClient (mainloopIn, mainloopOut) connection)

connSender :: Socket -> SockAddr -> TChan String -> IO ()
connSender connection address mainloopOut = forever $ do
    toSend <- atomically $ readTChan mainloopOut
    putStrLn toSend
    sendAll connection (pack (toSend ++ "\n"))

serverMainloop :: ServerInterface -> IO ()
serverMainloop (mainloopIn, mainloopOut) = forever $ do
    message <- atomically $ readTChan mainloopIn
    atomically $ writeTChan mainloopOut message