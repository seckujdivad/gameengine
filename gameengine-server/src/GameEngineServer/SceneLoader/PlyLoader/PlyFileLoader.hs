module GameEngineServer.SceneLoader.PlyLoader.PlyFileLoader (generatePLYFile, PLYFileGenerationError (..), PLYFileGenerationErrorInner (..), PLYFile (..), Element (..), Property (..), Value (..)) where

import Data.Maybe (mapMaybe)

import Data.Map (Map, empty, update, lookup, keys)

import Data.ByteString.Lazy (ByteString)

import GameEngineServer.SceneLoader.PlyLoader.PlyParser (ParseError, Parse (..), ElementType (..))

    
-- |Generates 'Geometry' from a parsed PLY file in the form of a list of 'Parse' with line indices
generatePLYFile :: [(Int, Parse)] -> Either PLYFile PLYFileGenerationError
generatePLYFile parses = case null parseErrors of
        True -> generatePLYFileInner parses initialState
        False -> Right $ ParsingErrors parseErrors
    where
        parseErrors = mapMaybe (\(lineNumber, parse) -> case parse of
            Malformed parseError -> Just (lineNumber, parseError)
            _ -> Nothing) parses

generatePLYFileInner :: [(Int, Parse)] -> GeometryGeneratorState -> Either PLYFile PLYFileGenerationError
generatePLYFileInner [] state = case state of
    EndOfFile plyFile -> Left $ plyFile
    _ -> Right $ PLYFileGenerationError Nothing UnexpectedEOF
generatePLYFileInner ((lineNumber, parse):remainingParses) state = case state of
    Preamble fileTypeSeen formatSeen -> case parse of
        FileType -> if fileTypeSeen then
                generateError $ BadPreamble "FileType appeared twice in the preamble"
            else (if formatSeen then
                    generateError $ BadPreamble "Format was seen before FileType"
                else
                    doNextParse (Preamble True formatSeen))
        Format isCorrect -> if (not fileTypeSeen) || formatSeen then
                generateError $ BadPreamble $ (if not fileTypeSeen then "Format appeared before FileType in the preamble" else "") ++ (if not fileTypeSeen && formatSeen then ". " else "") ++ (if formatSeen then "Format appeared twice in the preamble" else "")
            else (if isCorrect then
                    doNextParse (Header [])
                else
                    generateError $ BadPreamble "Incorrect format in preamble")
        _ ->  generateError $ BadPreamble "Preamble must only contain FileType and Format"
    
    Header headerElements -> case parse of
        Element name count -> if count >= 0 then
                doNextParse $ Header (headerElements ++ [HeaderElement name (fromIntegral count) []])
            else
                generateError $ BadHeader "Negative numbers of elements can't be provided"

        Property name propType -> doNextParse $ Header $ modifyLast (\(HeaderElement elementName count properties) -> HeaderElement elementName count (properties ++ [HeaderValueProperty name (elementTypeToHeaderValue propType)])) headerElements

        GameEngineServer.SceneLoader.PlyLoader.PlyParser.ListProperty name sizeType propType -> case elementTypeToHeaderValue sizeType of
            HeaderIntegerValue -> doNextParse $ Header $ modifyLast (\(HeaderElement elementName count properties) -> HeaderElement elementName count (properties ++ [HeaderValueProperty name (elementTypeToHeaderValue propType)])) headerElements
            HeaderDoubleValue -> generateError $ BadHeader "ListProperty size type must be an integer variant"

        EndHeader -> doNextParse $ GameEngineServer.SceneLoader.PlyLoader.PlyFileLoader.Body headerElements 0 0 emptyPLYFile

        _ -> generateError $ BadHeader "Header must only contain Element, Property, ListProperty or EndHeader"
    
    GameEngineServer.SceneLoader.PlyLoader.PlyFileLoader.Body headerElements currentElementIndex numOfElementProcessed (PLYFile elementMap) -> case parse of
        GameEngineServer.SceneLoader.PlyLoader.PlyParser.Body byteStrings -> generateError $ ExpectedEOF -- TODO
            where
                currentElement = headerElements !! currentElementIndex
        _ -> generateError $ BadBody "Only Body is allowed in body"
    
    EndOfFile _ -> generateError $ ExpectedEOF

    where
        generateError error = Right $ PLYFileGenerationError (Just lineNumber) error
        doNextParse = generatePLYFileInner remainingParses
        

data PLYFileGenerationError =
    ParsingErrors [(Int, ParseError)]
    | PLYFileGenerationError (Maybe Int) PLYFileGenerationErrorInner

instance Show PLYFileGenerationError where
    show (ParsingErrors errors) = if null errors then
            "Parsing errors"
        else
            "Parsing errors:\n" ++ (concatMap 
                (\(lineNumber, parseError) -> "Line " ++ show lineNumber ++ ": " ++ show parseError)
                errors)
    show (PLYFileGenerationError lineNumberMaybe errorInner) = (case lineNumberMaybe of
        Just lineNumber -> "Line " ++ show lineNumber ++ ": "
        Nothing -> "") ++ show errorInner

data PLYFileGenerationErrorInner =
    UnexpectedEOF
    | BadPreamble String
    | BadHeader String
    | BadBody String
    | ExpectedEOF
    deriving (Show)

data GeometryGeneratorState =
    -- |Preamble of the PLY file. First 'Bool' is whether or not 'FileType' has been seen, second is the same but for 'Format'
    Preamble Bool Bool
    | Header [HeaderElement]
    | Body [HeaderElement] Int Int PLYFile
    -- |The end of the file is expected
    | EndOfFile PLYFile

initialState :: GeometryGeneratorState
initialState = Preamble False False

data HeaderElement = HeaderElement ByteString Int [HeaderProperty]
data HeaderProperty = HeaderValueProperty ByteString HeaderValue | HeaderListProperty ByteString [HeaderValue]
data HeaderValue = HeaderDoubleValue | HeaderIntegerValue

elementTypeToHeaderValue :: ElementType -> HeaderValue
elementTypeToHeaderValue elementType = case elementType of
    FloatingPoint -> HeaderDoubleValue
    Integer -> HeaderIntegerValue
    UnsignedInteger -> HeaderIntegerValue


data PLYFile = PLYFile (Map ByteString Element)
data Element = Map ByteString Property
data Property = ValueProperty ByteString Value | ListProperty ByteString [Value]
data Value = DoubleValue Double | IntegerValue Integer

emptyPLYFile :: PLYFile
emptyPLYFile = PLYFile empty

modifyLast :: (a -> a) -> [a] -> [a]
modifyLast f xs = (init xs) ++ [f (last xs)]