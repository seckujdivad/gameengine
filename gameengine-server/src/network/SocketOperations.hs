module SocketOperations (sendPacket, closeConnection) where

{-
Simplifies common operations of 'Socket's.
-}

import Network.Socket (Socket, gracefulClose)
import Network.Socket.ByteString (sendAll)

import Control.Exception (catch, IOException)

import Packet (Packet, serialise)


-- |Sends a 'Packet' to a 'Socket'. Returns 'Nothing' if the 'Packet' was sent without error, 'Just String' otherwise
sendPacket :: Socket -> Packet -> IO (Maybe String)
sendPacket socket packet = catch (do
        sendPacketInner socket packet
        return Nothing) (\error -> return (Just (show (error :: IOException))))

-- |Sends a 'Packet' to a 'Socket'
sendPacketInner :: Socket -> Packet -> IO ()
sendPacketInner socket = sendAll socket . serialise

-- |Close a 'Socket'
closeConnection :: Socket -> IO ()
closeConnection connection = catch (gracefulClose connection 5000) (const (return ()) :: IOException -> IO ()) --throws if the connection isn't active