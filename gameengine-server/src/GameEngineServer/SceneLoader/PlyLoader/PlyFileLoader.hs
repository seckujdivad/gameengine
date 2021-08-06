module GameEngineServer.SceneLoader.PlyLoader.PlyFileLoader (generatePLYFile, PLYFileGenerationError (..), PLYFileGenerationErrorInner (..), PLYFile (..), Element (..), Property (..), Value (..)) where

import Data.Maybe (mapMaybe)

import Data.Map (Map, empty, update, lookup, keys, fromList, insert)

import qualified Data.ByteString.Lazy as LBS (ByteString, null, fromStrict, fromStrict, toStrict)
import Data.ByteString.Lex.Fractional (readDecimal, readSigned)

import qualified GameEngineServer.SceneLoader.PlyLoader.PlyParser as PlyParser (ParseError, Parse (..), ElementType (..))

    
-- |Generates 'Geometry' from a parsed PLY file in the form of a list of 'Parse' with line indices
generatePLYFile :: [(Int, PlyParser.Parse)] -> Either PLYFile PLYFileGenerationError
generatePLYFile parses = case null parseErrors of
        True -> generatePLYFileInner parses initialState
        False -> Right $ ParsingErrors parseErrors
    where
        parseErrors = mapMaybe (\(lineNumber, parse) -> case parse of
            PlyParser.Malformed parseError -> Just (lineNumber, parseError)
            _ -> Nothing) parses

generatePLYFileInner :: [(Int, PlyParser.Parse)] -> GeometryGeneratorState -> Either PLYFile PLYFileGenerationError
generatePLYFileInner [] state = case state of
    EndOfFile plyFile -> Left $ plyFile
    _ -> Right $ PLYFileGenerationError Nothing UnexpectedEOF
generatePLYFileInner ((lineNumber, parse):remainingParses) state = case state of
    Preamble fileTypeSeen formatSeen -> case parse of
        PlyParser.FileType -> if fileTypeSeen then
                generateError $ BadPreamble "FileType appeared twice in the preamble"
            else (if formatSeen then
                    generateError $ BadPreamble "Format was seen before FileType"
                else
                    doNextParse (Preamble True formatSeen))
        PlyParser.Format isCorrect -> if (not fileTypeSeen) || formatSeen then
                generateError $ BadPreamble $ (if not fileTypeSeen then "Format appeared before FileType in the preamble" else "") ++ (if not fileTypeSeen && formatSeen then ". " else "") ++ (if formatSeen then "Format appeared twice in the preamble" else "")
            else (if isCorrect then
                    doNextParse (Header [])
                else
                    generateError $ BadPreamble "Incorrect format in preamble")
        _ ->  generateError $ BadPreamble "Preamble must only contain FileType and Format"
    
    Header headerElements -> case parse of
        PlyParser.Element name count -> case count >= 0 of
            True -> doNextParse $ Header (headerElements ++ [HeaderElement name (fromIntegral count) []])
            False -> generateError $ BadHeader "Negative numbers of elements can't be provided"

        PlyParser.Property name propType -> doNextParse $ Header $ modifyLast (\(HeaderElement elementName count properties) -> HeaderElement elementName count (properties ++ [HeaderValueProperty name (elementTypeToHeaderValue propType)])) headerElements

        PlyParser.ListProperty name sizeType propType -> case elementTypeToHeaderValue sizeType of
            HeaderIntegerValue -> doNextParse $ Header $ modifyLast (\(HeaderElement elementName count properties) -> HeaderElement elementName count (properties ++ [HeaderListProperty name (elementTypeToHeaderValue propType)])) headerElements
            HeaderDoubleValue -> generateError $ BadHeader "ListProperty size type must be an integer variant"

        PlyParser.EndHeader -> doNextParse $ Body headerElements 0 emptyPLYFile

        _ -> generateError $ BadHeader "Header must only contain Element, Property, ListProperty or EndHeader"
    
    Body (headerElement:headerElements) numOfElementProcessed (PLYFile elementMap) -> case parse of
        PlyParser.Body byteStrings -> case loadElement lineNumber headerElement byteStrings of
                Left element -> case moveToNextElement of
                        True -> doNextParse $ Body (headerElement:headerElements) (numOfElementProcessed + 1) newPLyFile
                        False -> case null headerElements of
                            True -> doNextParse $ EndOfFile newPLyFile
                            False -> doNextParse $ Body headerElements 0 newPLyFile
                    where
                        newPLyFile = PLYFile (insert elementName element elementMap)
                        moveToNextElement = numOfElementProcessed + 1 >= elementCount
                
                Right error -> Right error
            where
                HeaderElement elementName elementCount _ = headerElement
        
        _ -> generateError $ BadBody "Only Body is allowed in body"
    
    Body [] _ _ -> generateError $ BadBody "Invalid state - shouldn't be in Body state with no header elements still to be processed"
    
    EndOfFile _ -> generateError $ ExpectedEOF

    where
        generateError error = Right $ PLYFileGenerationError (Just lineNumber) error
        doNextParse = generatePLYFileInner remainingParses
        

data PLYFileGenerationError =
    ParsingErrors [(Int, PlyParser.ParseError)]
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
    | ValueUnparseable String
    deriving (Show)

data GeometryGeneratorState =
    -- |Preamble of the PLY file. First 'Bool' is whether or not 'FileType' has been seen, second is the same but for 'Format'
    Preamble Bool Bool
    | Header [HeaderElement]
    | Body [HeaderElement] Int PLYFile
    -- |The end of the file is expected
    | EndOfFile PLYFile

initialState :: GeometryGeneratorState
initialState = Preamble False False

data HeaderElement = HeaderElement LBS.ByteString Int [HeaderProperty]
data HeaderProperty = HeaderValueProperty LBS.ByteString HeaderValue | HeaderListProperty LBS.ByteString HeaderValue
data HeaderValue = HeaderDoubleValue | HeaderIntegerValue

elementTypeToHeaderValue :: PlyParser.ElementType -> HeaderValue
elementTypeToHeaderValue elementType = case elementType of
    PlyParser.FloatingPoint -> HeaderDoubleValue
    PlyParser.Integer -> HeaderIntegerValue
    PlyParser.UnsignedInteger -> HeaderIntegerValue

data PLYFile = PLYFile (Map LBS.ByteString Element)
data Element = Element (Map LBS.ByteString Property)
data Property = ValueProperty Value | ListProperty [Value]
data Value = DoubleValue Double | IntegerValue Integer

emptyPLYFile :: PLYFile
emptyPLYFile = PLYFile empty

modifyLast :: (a -> a) -> [a] -> [a]
modifyLast f xs = (init xs) ++ [f (last xs)]

loadElement :: Int -> HeaderElement -> [LBS.ByteString] -> Either Element PLYFileGenerationError
loadElement lineNumber (HeaderElement name _ properties) bodyByteStrings = case loadedElementByteStringOrError of
        Left (element, remainingByteString) -> case null remainingByteString of
            True -> Left $ element
            False -> Right $ PLYFileGenerationError (Just lineNumber) $ BadBody "Too many values/not enough properties"
        Right error -> Right error
    where
        loadedElementByteStringOrError = foldl (loadElementInner lineNumber) (Left $ (Element empty, bodyByteStrings)) properties


loadElementInner :: Int -> Either (Element, [LBS.ByteString]) PLYFileGenerationError -> HeaderProperty -> Either (Element, [LBS.ByteString]) PLYFileGenerationError
loadElementInner _ (Right error) _ = Right error
loadElementInner lineNumber (Left (element, byteStrings)) headerProperty = case headerProperty of
        HeaderValueProperty name valueType -> case null byteStrings of
                True -> generateError $ BadBody "Can't read value property - no more values left"
                False -> case loadValue lineNumber valueType (head byteStrings) of
                    Left value -> Left $ (Element $ insert name (ValueProperty value) propertyMap, tail byteStrings)
                    Right error -> Right error
            where
                Element propertyMap = element
        HeaderListProperty name valueType -> case null byteStrings of
            True -> generateError $ BadBody "Can't read size of list - no more values"
            False -> case loadValue lineNumber HeaderIntegerValue $ head byteStrings of
                Left (IntegerValue listSize) -> case length byteStrings >= (intListSize + 1) of
                        True -> case null errors of
                                True -> Left $ (Element $ insert name (ListProperty values) propertyMap, remainingByteStrings)
                                    where
                                        Element propertyMap = element
                                        remainingByteStrings = drop (intListSize + 1) byteStrings

                                        values = mapMaybe (\valueOrError -> case valueOrError of
                                            Left value -> Just value
                                            Right error -> Nothing
                                            ) valuesOrErrors

                                False -> Right $ head errors
                            where
                                valuesOrErrors = map (loadValue lineNumber valueType) (take intListSize (tail byteStrings))

                                errors = mapMaybe (\valueOrError -> case valueOrError of
                                    Left value -> Nothing
                                    Right error -> Just error
                                    ) valuesOrErrors

                        False -> generateError $ BadBody $ "List size is " ++ show listSize ++ ", but there is/are only " ++ show (length byteStrings - 1) ++ " value(s) left"
                    where
                        intListSize = fromIntegral listSize
                Left (DoubleValue _) -> Prelude.error "loadValue returned double when the type was specified as integer"
                Right error -> Right error
    where
        generateError error = Right $ PLYFileGenerationError (Just lineNumber) error

loadValue :: Int -> HeaderValue -> LBS.ByteString -> Either Value PLYFileGenerationError
loadValue lineNumber HeaderDoubleValue byteString = case (readSigned readDecimal) $ LBS.toStrict byteString of
    Just (value, remainingByteString) -> case LBS.null $ LBS.fromStrict remainingByteString of
        True -> Left $ DoubleValue value
        False -> Right $ PLYFileGenerationError (Just lineNumber) $ ValueUnparseable "Bytes were left over after parsing value"
    Nothing -> Right $ PLYFileGenerationError (Just lineNumber) $ ValueUnparseable "Couldn't parse value"

loadValue lineNumber HeaderIntegerValue byteString = case (readSigned readDecimal) $ LBS.toStrict byteString of
    Just (value, remainingByteString) -> case LBS.null $ LBS.fromStrict remainingByteString of
        True -> Left $ IntegerValue $ round value
        False -> Right $ PLYFileGenerationError (Just lineNumber) $ ValueUnparseable "Bytes were left over after parsing value"
    Nothing -> Right $ PLYFileGenerationError (Just lineNumber) $ ValueUnparseable "Couldn't parse value"