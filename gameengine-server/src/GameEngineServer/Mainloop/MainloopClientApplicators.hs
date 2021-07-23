module GameEngineServer.Mainloop.MainloopClientApplicators (sendPacketToClient, closeClient) where

import GameEngineServer.ServerState.ClientApplicators (ClientApplicator (..))
import GameEngineServer.ServerState.Client (Client (Client))
import GameEngineServer.Network.Packet (Packet (..))
import GameEngineServer.Network.SocketOperations (sendPacket)
import GameEngineServer.Network.TCPServer (ConnInfo (ConnInfo))


closeClient :: ClientApplicator
closeClient = ClientApplicator $ \_ -> return $ Left $ Nothing

sendPacketToClient :: Packet -> ClientApplicator
sendPacketToClient packet  = ClientApplicator $ \client -> do
    let Client connection _ _ = client
    errorTextMaybe <- sendPacket connection packet
    case errorTextMaybe of
        Just errorText -> return $ Right errorText
        Nothing -> return $ Left $ Just client