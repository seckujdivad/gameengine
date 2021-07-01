module MainloopClientApplicators (sendPacketToClient, closeClient) where

import ClientApplicators (ClientApplicator)
import Packet (Packet (..))
import SocketOperations (sendPacket)
import Client (Client (Client))
import TCPServer (ConnInfo (ConnInfo))

closeClient :: ClientApplicator
closeClient _ = return $ Left $ Nothing

sendPacketToClient :: Packet -> ClientApplicator
sendPacketToClient packet client = do
    let Client connection _ _ = client
    errorTextMaybe <- sendPacket connection packet
    case errorTextMaybe of
        Just errorText -> return $ Right errorText
        Nothing -> return $ Left $ Just client