{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.PlyLoader (loadPolygonalFromPLY) where

import Data.ByteString.Lazy (ByteString, reverse, null, tail, head, pack, unpack)
import Data.ByteString.Lazy.Char8 (splitWith, head, readInteger)

import Data.Maybe (catMaybes)

import GameEngineServer.State.Scene.Model.Model (Model (..))
import GameEngineServer.State.Scene.Model.Geometry (Geometry (Polygonal), Face (Face))

{-
check preamble
load header
load body

lexer
-}

loadPolygonalFromPLY :: ByteString -> Maybe Geometry
loadPolygonalFromPLY file = Just $ Polygonal []
    where
        parsedFile = parser file

data Parse = Malformed ParseError
    | FileType
    | Format Bool
    | Element ByteString Integer
    | Property ByteString ElementType
    | ListProperty ByteString ElementType ElementType
    | EndHeader
    | Body [ByteString]

data ElementType = FloatingPoint | Integer | UnsignedInteger deriving (Show, Eq, Enum)

data ParseError =
    UnrecognisedLeadToken String
    | WrongNumberOfTokens String
    | IntegerUnparseable
    | UnknownType
    | TokenRequired String
    | WrongType String
    deriving (Show)

-- |Parse a 'ByteString' containing a PLY file into a list of 'Parse' objects representing every meaningful line alongside their line number
parser :: ByteString -> [(Int, Parse)]
parser file = catMaybes parsedLinesWithIndexMaybes
    where
        linesWithReturn = splitWith (== '\n') file --might contain carriage returns
        hasCarriageReturn = or $ map (byteStringEndsInChar '\r') linesWithReturn
        lines = if hasCarriageReturn then map (\line -> if Data.ByteString.Lazy.null line then line else Data.ByteString.Lazy.reverse $ Data.ByteString.Lazy.tail $ Data.ByteString.Lazy.reverse line) linesWithReturn else linesWithReturn

        parsedLineMaybes = map parseLine lines
        parsedLinesWithIndexMaybes :: [Maybe (Int, Parse)]
        parsedLinesWithIndexMaybes = mapWithIndex (\(lineNum, parseMaybe) -> case parseMaybe of
            Just parse -> Just (lineNum, parse)
            Nothing -> Nothing) parsedLineMaybes

-- |Check if the last element in a 'ByteString' is the given 'Char'. If the 'ByteString' is empty, return 'False'
byteStringEndsInChar :: Char -> ByteString -> Bool
byteStringEndsInChar c bs = if Data.ByteString.Lazy.null bs then False else c == Data.ByteString.Lazy.Char8.head (Data.ByteString.Lazy.reverse bs)

-- |Parse one line from a PLY file
parseLine :: ByteString -> Maybe Parse
parseLine line = if Prelude.null $ splitOnSpaces then
        Nothing
    else
        case Prelude.head splitOnSpaces of
            "" -> Nothing

            "ply" -> if length splitOnSpaces == 1 then
                    Just FileType
                else
                    Just $ Malformed $ WrongNumberOfTokens "When the first token is ply, there must be no other tokens"
            
            "format" -> Just $ if length splitOnSpaces == 3 then
                    Format (splitOnSpaces !! 1 == "ascii" && splitOnSpaces !! 2 == "1.0")
                else
                    Malformed $ WrongNumberOfTokens "When the first token is format, there must be exactly 2 other tokens"
            
            "comment" -> Nothing

            "element" -> Just $ if length splitOnSpaces == 3 then
                    case readInteger (splitOnSpaces !! 2) of
                        Just (numElements, remaining) -> if Data.ByteString.Lazy.null remaining then Element (splitOnSpaces !! 1) numElements else Malformed IntegerUnparseable
                        Nothing -> Malformed IntegerUnparseable
                else
                    Malformed $ WrongNumberOfTokens "When the first token is element, there must be exactly 2 other tokens"
            
            "property" -> case length splitOnSpaces of
                3 -> case getType (splitOnSpaces !! 1) of
                    Just elementType -> Just $ Property (splitOnSpaces !! 2) elementType
                    Nothing -> Just $ Malformed $ UnknownType
                
                5 -> if splitOnSpaces !! 1 == "list" then
                        case getType (splitOnSpaces !! 2) of
                            Just sizeType -> if sizeType == Integer || sizeType == UnsignedInteger then
                                    case getType (splitOnSpaces !! 3) of
                                        Just elementType -> Just $ ListProperty (splitOnSpaces !! 4) sizeType elementType
                                        Nothing -> Just $ Malformed $ UnknownType
                                else
                                    Just $ Malformed $ WrongType "List sizes must be denoted as integers or unsigned integers"
                            Nothing -> Just $ Malformed $ UnknownType
                    else
                        Just $ Malformed $ TokenRequired "If property has 5 elements, the second token must be list"

                _ -> Just $ Malformed $ WrongNumberOfTokens "When the first token is property, there must be either 3 of 5 other tokens" 

            "end_header" -> if length splitOnSpaces == 1 then
                    Just EndHeader
                else
                    Just $ Malformed $ WrongNumberOfTokens "When the first token is end_header, there must be no other tokens"
            
            -- the only other thing that could be correct is if this line was an array of numbers (ie part of the body)
            _ -> Just $ Body splitOnSpaces
    
    where
        splitOnSpaces = splitWith (== ' ') line

-- |Determines the 'ElementType' of a type name given in a PLY file
getType :: ByteString -> Maybe ElementType
getType identifier = case identifier of
    "char" -> Just Integer
    "uchar" -> Just UnsignedInteger
    "short" -> Just Integer
    "ushort" -> Just UnsignedInteger
    "int" -> Just Integer
    "uint" -> Just UnsignedInteger
    "float" -> Just FloatingPoint
    "double" -> Just FloatingPoint
    _ -> Nothing

-- |Map a list and provide the index of each element being mapped to the provided function
mapWithIndex :: Num a => Enum a => ((a, b) -> c) -> [b] -> [c]
mapWithIndex f xs = map f $ zip [0..] xs