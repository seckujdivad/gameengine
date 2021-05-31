module Packet (Packet (ConnEstablished, ChatMessage), serialise, deserialise) where

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import Data.ByteString.Char8 (pack, unpack, readInteger)
import Data.ByteString.Builder (toLazyByteString, Builder, string8, word8, int32LE, char8)
import Data.Word (Word8)
import Data.Int (Int32)
import Data.Char (chr)
import Data.Binary.Get (Get, runGet, getWord8, getInt32le, getRemainingLazyByteString)


-- |Type used to represent information being sent between the server and the client
data Packet =
    -- |Sent from server to client containing the unique ID of that client when the client connects
    ConnEstablished Integer
    -- |Sent by either the client or the server containing global chat messages
    | ChatMessage String

instance Show Packet where
    show (ConnEstablished uid) = "ConnEstablished {uid: " ++ show uid ++ "}"
    show (ChatMessage message) = "ChatMessage {message: \"" ++ show message ++ "\"}"

-- |Turn a 'Packet' into a 'ByteString' with the correct header and delimiter ready to be broadcasted
serialise :: Packet -> Data.ByteString.ByteString
serialise packet = if body_length > (2 ^ 8) then error "Body must contain less than 256 bytes" else (Data.ByteString.Lazy.toStrict . Data.ByteString.Lazy.concat) [header, body]
    where
        body = (toLazyByteString . serialiseInner) packet
        body_length = Data.ByteString.Lazy.length body
        header = (toLazyByteString . word8) (fromIntegral body_length)

serialiseInner :: Packet -> Builder
serialiseInner (ConnEstablished uid) = mconcat [word8 0, int32LE (fromIntegral uid)]
serialiseInner (ChatMessage message) = mconcat [word8 1, string8 message]

-- |Turn a 'ByteString' into a 'Packet' if it conforms to the correct layout
deserialise :: Data.ByteString.ByteString -> Packet
deserialise bytes = runGet getPacket (Data.ByteString.Lazy.fromStrict bytes)

-- |Get a 'Packet' from a lazy 'ByteString' using 'runGet'
getPacket :: Get Packet
getPacket = do
    packetType <- getWord8
    if packetType == 0 then do
        uid <- getInt32le
        return (ConnEstablished (fromIntegral uid))
    
    else if packetType == 1 then do
        message <- getRemainingString
        return (ChatMessage message)
    
    else
        error ("Unknown header - " ++ show packetType)

-- |Get the remaining 'Char's and pack into a 'String' from a lazy 'ByteString' using 'runGet'
getRemainingString :: Get String
getRemainingString = fmap (unpack . Data.ByteString.Lazy.toStrict) getRemainingLazyByteString