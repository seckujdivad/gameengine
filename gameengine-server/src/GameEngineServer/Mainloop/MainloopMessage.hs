module GameEngineServer.Mainloop.MainloopMessage (MainloopMessage (ReceiverMsg), ReceiverMsgInner (ClientConnEstablished, PackedReceived, ClientConnClosed, ReceiverException)) where

import Network.Socket (Socket)

import GameEngineServer.Network.Packet (Packet)
import GameEngineServer.Network.TCPServer (ConnInfo (ConnInfo))


-- |Message sent to the main thread
data MainloopMessage =
    -- |Message sent from a receiver thread to the main thread
    ReceiverMsg ConnInfo ReceiverMsgInner

instance Show MainloopMessage where
    show (ReceiverMsg (ConnInfo uid _) inner) = show uid ++ ": " ++ show inner

-- |Body of a 'ReceiverMsg'
data ReceiverMsgInner =
    -- |Sent when a new connection is made
    ClientConnEstablished Socket
    -- |Sent when a 'Packet' is received and successfully parsed
    | PackedReceived Packet
    -- |Sent when the connection is closed without error
    | ClientConnClosed
    -- |Sent when an error occurs in the receiver thread
    | ReceiverException String
    deriving (Show)