{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.PlyLoader.PlyParser (plyParser, Parse (..), ElementType (..), ParseError (..)) where

import Prelude hiding (lines)

import Data.ByteString.Lazy (ByteString, reverse, null, tail)
import Data.ByteString.Lazy.Char8 (splitWith, head, readInteger)

import Data.Maybe (catMaybes)

{-
Whenever something is referred to as a token, it refers to the groups of characters resulting from a line being split into groups of (including zero) of characters separated by a " ". A token can have zero length, but this isn't always valid in every situation.
-}

-- |Represents a parsed single line from a PLY file
data Parse =
    -- |A line that could not be parsed
    Malformed ParseError
    -- |The "ply" line from the header
    | FileType
    -- |The "format ascii 1.0". Contains a 'Bool' that specifies whether or not it is correct for this loader (i.e. "format ascii 1.0").
    | Format Bool
    -- |The start of an element declaration. Stores the name of the element and the number of times it should appear in the body of the file.
    | Element ByteString Integer
    -- |The declaration of a property that contains only one value
    | Property ByteString ElementType
    -- |The declaration of a property that contains a list of values
    | ListProperty ByteString ElementType ElementType
    -- |The "end_header" line
    | EndHeader
    -- |A line from the body. Stores the line split into tokens based on " ". No validation of the tokens takes places at this stage and all lines that can't be parsed any other way are parsed as 'Body'.
    | Body [ByteString]

-- |Represents a type in a PLY file
data ElementType =
    -- |Represents any size of type storing real numbers
    FloatingPoint
    -- |Represents any size of type storing signed integers
    | Integer
    -- |Represents any size of type storing unsigned integers
    | UnsignedInteger deriving (Show, Eq, Enum)

-- |Represents an error that may occur while parsing a PLY file
data ParseError =
    -- |More tokens than expected given the tokens already processed
    WrongNumberOfTokens String
    -- |A token expected to be an integer in the file couldn't be read
    | IntegerUnparseable
    -- |A token that was expected to be one of the PLY preset types wasn't recognised
    | UnknownType
    -- |A specific token which must have an exact value to be valid was of a different value (and therefore invalid)
    | TokenRequired String
    -- |A certain family of type must be specified in this situation, but it was not
    | WrongType String
    -- |A line was started with " ", producing a token of zero length (which is not allowed)
    | NoStartingLineWithSpace
    deriving (Show)

-- |Parse a 'ByteString' containing a PLY file into a list of 'Parse' objects representing every meaningful line alongside their line number
plyParser :: ByteString -> [(Int, Parse)]
plyParser file = catMaybes parsedLinesWithIndexMaybes
    where
        linesWithReturn = splitWith (== '\n') file --might contain carriage returns
        hasCarriageReturn = or $ map (byteStringEndsInChar '\r') linesWithReturn -- check file for carriage returns
        lines = if hasCarriageReturn then map (\line -> if Data.ByteString.Lazy.null line then line else Data.ByteString.Lazy.reverse $ Data.ByteString.Lazy.tail $ Data.ByteString.Lazy.reverse line) linesWithReturn else linesWithReturn --strip off carriage return if the file had it

        parsedLineMaybes = map parseLine lines
        parsedLinesWithIndexMaybes :: [Maybe (Int, Parse)]
        parsedLinesWithIndexMaybes = mapWithIndex (\(lineNum, parseMaybe) -> case parseMaybe of
            Just parse -> Just (lineNum + 1, parse)
            Nothing -> Nothing) parsedLineMaybes

-- |Check if the last element in a 'ByteString' is the given 'Char'. If the 'ByteString' is empty, return 'False'
byteStringEndsInChar :: Char -> ByteString -> Bool
byteStringEndsInChar c bs = if Data.ByteString.Lazy.null bs then False else c == Data.ByteString.Lazy.Char8.head (Data.ByteString.Lazy.reverse bs)

-- |Parse one line from a PLY file
parseLine :: ByteString -> Maybe Parse
parseLine line = if Prelude.null $ splitOnSpaces then
        Nothing -- ignore empty lines
    else
        case Prelude.head splitOnSpaces of
            "" -> Just $ Malformed NoStartingLineWithSpace

            "ply" -> if length splitOnSpaces == 1 then
                    Just FileType
                else
                    Just $ Malformed $ WrongNumberOfTokens "When the first token is ply, there must be no other tokens"
            
            "format" -> Just $ if length splitOnSpaces == 3 then
                    Format (splitOnSpaces !! 1 == "ascii" && splitOnSpaces !! 2 == "1.0")
                else
                    Malformed $ WrongNumberOfTokens "When the first token is format, there must be exactly 2 other tokens"
            
            "comment" -> Nothing -- ignore comments

            "element" -> Just $ if length splitOnSpaces == 3 then
                    case readInteger (splitOnSpaces !! 2) of
                        Just (numElements, remaining) -> if Data.ByteString.Lazy.null remaining then Element (splitOnSpaces !! 1) numElements else Malformed IntegerUnparseable
                        Nothing -> Malformed IntegerUnparseable
                else
                    Malformed $ WrongNumberOfTokens "When the first token is element, there must be exactly 2 other tokens"
            
            "property" -> case length splitOnSpaces of
                3 -> case getType (splitOnSpaces !! 1) of -- single value property
                    Just elementType -> Just $ Property (splitOnSpaces !! 2) elementType
                    Nothing -> Just $ Malformed $ UnknownType
                
                5 -> if splitOnSpaces !! 1 == "list" then -- list property
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