module GameEngineServer.Network.Packet (Packet (..), serialise, deserialise, PacketType (..), packetToPacketType) where

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import Data.ByteString.Char8 (pack, unpack, readInteger)
import Data.ByteString.Builder (toLazyByteString, Builder, string8, word8, int32LE, char8)
import Data.Word (Word8)
import Data.Int (Int32)
import Data.Char (chr)
import Data.Binary.Get (Get, runGet, getWord8, getInt32le, getRemainingLazyByteString)

{-
Checklist for adding new Packet types:
1. Add to Packet
2. Add to instance Show Packet
3. Add to serialiseInner
4. Add to getPacket
5. Add to PacketType
6. Add to packetToPacketType
-}

-- |Type used to represent information being sent between the server and the client
data Packet =
    -- |Sent from server to client containing the unique ID of that client when the client connects
    ConnEstablished Integer
    -- |Sent by either the client containing global chat messages
    | ClientChatMessage String
    -- |Sent by the server containing chat messages and the user who sent them
    | ServerChatMessage String String
    -- |Sent by a client containing its new name
    | SetClientName String
    -- |Sent by the server containing the new root and relative path to the scene file
    | SetScene String String

instance Show Packet where
    show (ConnEstablished uid) = "ConnEstablished {uid: " ++ show uid ++ "}"
    show (ClientChatMessage message) = "ClientChatMessage {message: \"" ++ message ++ "\"}"
    show (ServerChatMessage name message) = "ChatMessage {name: \"" ++ name ++ "\", " ++ "message: \"" ++ message ++ "\"}"
    show (SetClientName name) = "SetClientName {name: \"" ++ name ++ "\"}"
    show (SetScene root relative) = "SetClientName {root: \"" ++ root ++ "\", relative: \"" ++ relative ++ "\"}"

-- |Turn a 'Packet' into a 'ByteString' with the correct header and delimiter ready to be broadcasted
serialise :: Packet -> Data.ByteString.ByteString
serialise packet = if body_length > (2 ^ 8) then error "Body must contain less than 256 bytes" else (Data.ByteString.Lazy.toStrict . Data.ByteString.Lazy.concat) [header, body]
    where
        body = toLazyByteString (mappend ((word8 . packetToNum) packet) (serialiseInner packet))
        body_length = Data.ByteString.Lazy.length body
        header = (toLazyByteString . word8) (fromIntegral body_length)

serialiseInner :: Packet -> Builder
serialiseInner (ConnEstablished uid) = mconcat [int32LE (fromIntegral uid)]
serialiseInner (ClientChatMessage _) = error "Server can't ClientChatMessage"
serialiseInner (ServerChatMessage name message) = mconcat [string8 name, char8 '\0', string8 message]
serialiseInner (SetClientName _) = error "Server can't send SetClientName"
serialiseInner (SetScene root relative) = mconcat [string8 root, char8 '\0', string8 relative]

-- |Turn a 'ByteString' into a 'Packet' if it conforms to the correct layout
deserialise :: Data.ByteString.ByteString -> Packet
deserialise bytes = runGet getPacket (Data.ByteString.Lazy.fromStrict bytes)

-- |Get a 'Packet' from a lazy 'ByteString' using 'runGet'
getPacket :: Get Packet
getPacket = do
    packetType <- getWord8
    case numToPacketType packetType of
        TypeClientChatMessage -> do
            message <- getRemainingString
            return (ClientChatMessage message)
        
        TypeSetClientName -> do
            message <- getRemainingString
            return (SetClientName message)
    
        _ -> error ("Unknown header type received - " ++ show packetType)

-- |Get the remaining 'Char's and pack into a 'String' from a lazy 'ByteString' using 'runGet'
getRemainingString :: Get String
getRemainingString = fmap (unpack . Data.ByteString.Lazy.toStrict) getRemainingLazyByteString

-- |Enumerators representing constructors for 'Packet'
data PacketType = TypeConnEstablished | TypeClientChatMessage | TypeServerChatMessage | TypeSetClientName | TypeSetScene deriving (Show, Eq, Enum)

-- |Convert a 'Packet' to a 'PacketType' enumerator
packetToPacketType :: Packet -> PacketType
packetToPacketType (ConnEstablished _) = TypeConnEstablished
packetToPacketType (ClientChatMessage _) = TypeClientChatMessage
packetToPacketType (ServerChatMessage _ _) = TypeServerChatMessage
packetToPacketType (SetClientName _) = TypeSetClientName
packetToPacketType (SetScene _ _) = TypeSetScene

-- |Convert a 'PacketType' to some 'Num' that can be used when serialising and deserialising
packetTypeToNum :: Num a => PacketType -> a
packetTypeToNum packetType = fromIntegral $ fromEnum packetType

-- |Convert a 'Packet' to some 'Num' that can be used when serialising and deserialising
packetToNum :: Num a => Packet -> a
packetToNum = packetTypeToNum . packetToPacketType

-- |Convert 'Integral' (i.e. 'Word8') at the start of a raw 'Packet' into a 'PacketType'
numToPacketType :: Integral a => a -> PacketType
numToPacketType n = toEnum $ fromIntegral n