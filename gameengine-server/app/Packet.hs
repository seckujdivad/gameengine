module Packet (Packet (ConnEstablished, ChatMessage), serialise, deserialise) where

import qualified Data.ByteString
import Data.ByteString.Char8 (pack, unpack, readInteger)
import Data.ByteString.Builder (toLazyByteString, Builder, string8, word8, int32LE, char8)
import qualified Data.ByteString.Lazy
import Data.Word (Word8)
import Data.Int (Int32)


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
        body = (toLazyByteString . mconcat) (serialiseInner packet)
        body_length = Data.ByteString.Lazy.length body
        header = (toLazyByteString . word8) (fromIntegral body_length :: Word8)

serialiseInner :: Packet -> [Builder]
serialiseInner (ConnEstablished uid) = [word8 (0 :: Word8), int32LE (fromIntegral uid :: Int32)]
serialiseInner (ChatMessage message) = [word8 (1 :: Word8), string8 message]

-- |Turn a 'ByteString' into a 'Packet' if it conforms to the correct layout
deserialise :: Data.ByteString.ByteString -> Maybe Packet
deserialise bytes = case Data.ByteString.head bytes of
        -- ConnEstablished
        0 -> case readInteger body of
            Just (uid, _) -> Just (ConnEstablished uid)
            Nothing -> Nothing

        -- ChatMessage
        1 -> Just (ChatMessage (unpack body))

    where
        body = Data.ByteString.drop 1 bytes