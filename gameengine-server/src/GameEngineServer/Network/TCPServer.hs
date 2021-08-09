module GameEngineServer.Network.TCPServer (runTCPServer, ConnInfo (..)) where

import Network.Socket (ServiceName, SocketType(Stream), AddrInfo(addrFlags, addrSocketType),
    HostName, defaultHints, getAddrInfo, AddrInfoFlag(AI_PASSIVE), Socket, SockAddr, Socket, HostName, ServiceName, withSocketsDo,
    AddrInfo (addrAddress, addrProtocol, addrFamily, addrSocketType),
    SocketOption (ReuseAddr), setSocketOption, withFdSocket, accept, bind, listen,
    setCloseOnExecIfNeeded, close, gracefulClose, socket)

import Control.Exception (bracket)
import Control.Concurrent (forkFinally)


-- |Store information about a connection
data ConnInfo =
    -- |A connection's unique ID and address
    ConnInfo {
        -- |Unique identifier given to all connections
        ciUID :: Integer,
        -- |Destination address of the connection
        ciAddress :: SockAddr
    }

instance Eq ConnInfo where
    (==) (ConnInfo uid1 _) (ConnInfo uid2 _) = uid1 == uid2

instance Show ConnInfo where
    show (ConnInfo uid address) = show uid ++ " - " ++ show address

-- |Starts a TCP listen server on the given address and port. 'connHandler' is started in a new thread for each new connection
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> ConnInfo -> IO ()) -> IO ()
runTCPServer host port connHandler = withSocketsDo $ do
    address <- resolveAddress host (Just port)
    bracket (startListen address) close (connAccepter connHandler 0) --listen to the given address and close the connection when the listen function exits

-- |Accept all connection requests and call the 
connAccepter :: (Socket -> ConnInfo -> IO ()) -> Integer -> Socket -> IO ()
connAccepter handlerFunc uid sock = do
    (connection, address) <- accept sock --accept all incoming connections
    let connInfo = ConnInfo uid address
    _ <- forkFinally (handlerFunc connection connInfo) (const $ gracefulClose connection 5000)
    connAccepter handlerFunc (uid + 1) sock

-- |Create a 'Socket' that is listening on the given address
startListen :: AddrInfo -> IO Socket
startListen address = do
    sock <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
    setSocketOption sock ReuseAddr 1
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock (addrAddress address)
    listen sock 1024
    return sock

-- |Constructs an 'AddrInfo' for connecting a 'Socket'
resolveAddress :: Maybe HostName -> Maybe ServiceName -> IO AddrInfo
resolveAddress host port = do
        infos <- getAddrInfo (Just hints) host port
        return (head infos)
    where
        hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}