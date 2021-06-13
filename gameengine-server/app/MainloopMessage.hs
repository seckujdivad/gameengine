module MainloopMessage (MainloopMessage (ReceiverMsg, UIMsg),
    ReceiverMsgInner (ClientConnEstablished, PackedReceived, ClientConnClosed, ReceiverException),
    UIMsgInner (ChatMessage)) where

import Network.Socket (Socket)

import Packet (Packet)

import TCPServer (ConnInfo (ConnInfo))

-- |Message sent from a receiver thread to the main thread
data MainloopMessage =
    -- |Message sent from a receiver thread to the main thread
    ReceiverMsg ConnInfo ReceiverMsgInner
    -- |
    | UIMsg UIMsgInner

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

-- |Body of a 'UIMsg'
data UIMsgInner =
    -- |Send a 'String' to all clients
    ChatMessage String