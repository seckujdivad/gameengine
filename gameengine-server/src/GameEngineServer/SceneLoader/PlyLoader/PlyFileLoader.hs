module GameEngineServer.SceneLoader.PlyLoader.PlyFileLoader (generatePLYFile, PLYFileGenerationError (..), PLYFileGenerationErrorInner (..), PLYFile (..), Element (..), Property (..), Value (..), getDoubleFromElement, getIntegerFromElement, getDoubleListFromElement, getIntegerListFromElement) where

import Data.Maybe (mapMaybe)

import Data.Map (empty, insert, adjust, member, lookup)

import Data.ByteString (ByteString, null)
import Data.ByteString.Lex.Fractional (readDecimal, readSigned)
import Data.ByteString.Char8 (readInteger)

import qualified GameEngineServer.SceneLoader.PlyLoader.PlyParser as PlyParser (ParseError, Parse (..), ElementType (..))
import GameEngineServer.SceneLoader.PlyLoader.PLYFile (PLYFile (..), Element (..), Property (..), Value (..), emptyPLYFile)

    
-- |Generates 'Geometry' from a parsed PLY file in the form of a list of 'Parse' with line indices
generatePLYFile :: [(Int, PlyParser.Parse)] -> Either PLYFile PLYFileGenerationError
generatePLYFile parses = case Prelude.null parseErrors of
        True -> generatePLYFileInner parses geometryGeneratorInitialState
        False -> Right $ ParsingErrors parseErrors
    where
        parseErrors = mapMaybe (\(lineNumber, parse) -> case parse of
            PlyParser.Malformed parseError -> Just (lineNumber, parseError)
            _ -> Nothing) parses

-- |Inner function of 'generatePLYFile'
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
        PlyParser.Format isCorrect -> case (not fileTypeSeen) || formatSeen of
                True -> generateError $ BadPreamble $ (if not fileTypeSeen then "Format appeared before FileType in the preamble" else "") ++ (if not fileTypeSeen && formatSeen then ". " else "") ++ (if formatSeen then "Format appeared twice in the preamble" else "")
                False -> case isCorrect of
                    True -> doNextParse (Header [])
                    False -> generateError $ BadPreamble "Incorrect format in preamble"
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
    
    Body (headerElement:headerElements) numOfElementProcessed plyFileWithoutElement -> case parse of
        PlyParser.Body byteStrings -> case loadElement lineNumber headerElement byteStrings of
                Left element -> case moveToNextElement of
                        True -> case Prelude.null headerElements of
                            True -> doNextParse $ EndOfFile plyFileWithElement
                            False -> doNextParse $ Body headerElements 0 plyFileWithElement
                        False -> doNextParse $ Body (headerElement:headerElements) (numOfElementProcessed + 1) plyFileWithElement
                    where
                        moveToNextElement = numOfElementProcessed + 1 >= elementCount

                        elementMapWithoutElement = plyfElements plyFileWithoutElement
                        elementMapHasElementName = member elementName elementMapWithoutElement
                        elementMapWithElement = case elementMapHasElementName of
                            True -> adjust (\elements -> elements ++ [element]) elementName elementMapWithoutElement
                            False -> insert elementName [element] elementMapWithoutElement
                        plyFileWithElement = PLYFile elementMapWithElement
                
                Right genError -> Right genError
            where
                HeaderElement elementName elementCount _ = headerElement
        
        _ -> generateError $ BadBody "Only Body is allowed in body"
    
    Body [] _ _ -> generateError $ BadBody "Invalid state - shouldn't be in Body state with no header elements still to be processed"
    
    EndOfFile _ -> generateError $ ExpectedEOF

    where
        generateError genError = Right $ PLYFileGenerationError (Just lineNumber) genError
        doNextParse = generatePLYFileInner remainingParses
        
-- |Represents an error encountered in the process of taking a .PLY file and turning it into a 'PLYFile'
data PLYFileGenerationError =
    -- |Passes through an error encountered by the parser along with the line number
    ParsingErrors [(Int, PlyParser.ParseError)]
    -- |Wraps an error encountered when taking an array of 'Parse' and turning it into 'PLYFile'. Also optionally stores the line number that the 'Parse' is logically associated with.
    | PLYFileGenerationError (Maybe Int) PLYFileGenerationErrorInner

instance Show PLYFileGenerationError where
    show (ParsingErrors errors) = if Prelude.null errors then
            "Parsing errors"
        else
            "Parsing errors:\n" ++ (concatMap 
                (\(lineNumber, parseError) -> "Line " ++ show lineNumber ++ ": " ++ show parseError)
                errors)
    show (PLYFileGenerationError lineNumberMaybe errorInner) = (case lineNumberMaybe of
        Just lineNumber -> "Line " ++ show lineNumber ++ ": "
        Nothing -> "") ++ show errorInner

-- |Represents an error encountered when taking an array of 'Parse' and turning it into 'PLYFile'
data PLYFileGenerationErrorInner =
    -- |The end of the file was unexpected in this state
    UnexpectedEOF
    -- |The preamble (format and version information) contained an error (described in the 'String')
    | BadPreamble String
    -- |The header (element and property declarations) contained an error (described in the 'String')
    | BadHeader String
    -- |The body (a collection of integers and real numbers, each line representing an element as declared in the header) contained an error (described in the 'String')
    | BadBody String
    -- |The end of the file was expected in this state, but didn't happen
    | ExpectedEOF
    -- |A value (in the body) couldn't be deserialised into the type specified in the header
    | ValueUnparseable String
    deriving (Show)

data GeometryGeneratorState =
    -- |Preamble of the PLY file. First 'Bool' is whether or not 'FileType' has been seen, second is the same but for 'Format'
    Preamble Bool Bool
    -- |Header. Contains all the 'HeaderElement's deserialised so far
    | Header [HeaderElement]
    -- |Body. Contains all the 'HeaderElement's deserialised in the header that have yet to be fully deserialised (i.e. the specified count satisfied) in the body, the number of times the first 'HeaderElement' has been deserialised from the body and the current state of the 'PLYFile' that elements are loaded into. Once the number of deserialised 'Element's matches the expected number given by the 'HeaderElement', the first 'HeaderElement' is removed from the list.
    | Body [HeaderElement] Int PLYFile
    -- |The end of the file is expected. Contains the 'PLYFile that the function will eventually return.
    | EndOfFile PLYFile

-- |Initial state of 'generatePLYFileInner'
geometryGeneratorInitialState :: GeometryGeneratorState
geometryGeneratorInitialState = Preamble False False

-- |Represents an element declaration from a .PLY file header. Contains the name of the element, the number of elements expected in the body and the properties of the element
data HeaderElement =
    -- |Represents an element declaration from a .PLY file header. Contains the name of the element, the number of elements expected in the body and the properties of the element
    HeaderElement ByteString Int [HeaderProperty]

-- |Represents a property declaration from a .PLY file header. Is logically the child of a 'HeaderElement'
data HeaderProperty =
    -- |Property that is a single value. Stores the name and type
    HeaderValueProperty ByteString HeaderValue
    -- |Property that is a list of values. Stores the name and type
    | HeaderListProperty ByteString HeaderValue

-- |Represents a type as specified in the header of a .PLY file
data HeaderValue =
    -- |Represents any .PLY type that represents real numbers
    HeaderDoubleValue
    -- |Represents .PLY types that represent integers but not real numbers
    | HeaderIntegerValue

-- |Converts between the broader family of parsed 'PlyParser.ElementType' to the narrower 'HeaderValue'
elementTypeToHeaderValue :: PlyParser.ElementType -> HeaderValue
elementTypeToHeaderValue elementType = case elementType of
    PlyParser.FloatingPoint -> HeaderDoubleValue
    PlyParser.Integer -> HeaderIntegerValue
    PlyParser.UnsignedInteger -> HeaderIntegerValue

-- |Applies a function to the final element in a list.
modifyLast :: (a -> a) -> [a] -> [a]
modifyLast f xs = (init xs) ++ [f (last xs)]

-- |Loads an 'Element' (or generates an appropriate error message) from a 'HeaderElement' and a list of 'LBS.ByteString' (each item in the list containing a value which should be used to construct the 'Element'). The list of 'LBS.ByteString' is parsed from a single line in the body of the .PLY file split on each " ". A line number must also be provided for error messages.
loadElement :: Int -> HeaderElement -> [ByteString] -> Either Element PLYFileGenerationError
loadElement lineNumber (HeaderElement _ _ properties) bodyByteStrings = case loadedElementByteStringOrError of
        Left (element, remainingByteString) -> case Prelude.null remainingByteString of
            True -> Left $ element
            False -> Right $ PLYFileGenerationError (Just lineNumber) $ BadBody "Too many values/not enough properties"
        Right genError -> Right genError
    where
        loadedElementByteStringOrError = foldl (loadElementInner lineNumber) (Left $ (Element empty, bodyByteStrings)) properties -- fold across each property (i.e. iterate over each 'Property' from left to right), greedily consuming the required amount of 'LBS.ByteString'

-- |Inner function for 'loadElement'. Applied as a fold to a list of 'HeaderProperty' that make up a 'HeaderElement', greedily consuming the list of 'LBS.ByteString' and producing an 'Element'.
loadElementInner :: Int -> Either (Element, [ByteString]) PLYFileGenerationError -> HeaderProperty -> Either (Element, [ByteString]) PLYFileGenerationError
loadElementInner _ (Right genError) _ = Right genError
loadElementInner lineNumber (Left (element, byteStrings)) headerProperty = case headerProperty of
        HeaderValueProperty name valueType -> case Prelude.null byteStrings of
                True -> generateError $ BadBody "Can't read value property - no more values left"
                False -> case loadValue lineNumber valueType (head byteStrings) of
                    Left value -> Left $ (Element $ insert name (ValueProperty value) propertyMap, tail byteStrings)
                    Right genError -> Right genError
            where
                Element propertyMap = element
        HeaderListProperty name valueType -> case Prelude.null byteStrings of
            True -> generateError $ BadBody "Can't read size of list - no more values"
            False -> case loadValue lineNumber HeaderIntegerValue $ head byteStrings of
                Left (IntegerValue listSize) -> case length byteStrings >= (intListSize + 1) of
                        True -> case Prelude.null errors of
                                True -> Left $ (Element $ insert name (ListProperty values) propertyMap, remainingByteStrings)
                                    where
                                        Element propertyMap = element
                                        remainingByteStrings = drop (intListSize + 1) byteStrings

                                        values = mapMaybe (\valueOrError -> case valueOrError of
                                            Left value -> Just value
                                            Right _ -> Nothing
                                            ) valuesOrErrors

                                False -> Right $ head errors
                            where
                                valuesOrErrors = map (loadValue lineNumber valueType) (take intListSize (tail byteStrings))

                                errors = mapMaybe (\valueOrError -> case valueOrError of
                                    Left _ -> Nothing
                                    Right genError -> Just genError
                                    ) valuesOrErrors

                        False -> generateError $ BadBody $ "List size is " ++ show listSize ++ ", but there is/are only " ++ show (length byteStrings - 1) ++ " value(s) left"
                    where
                        intListSize = fromIntegral listSize
                
                Left (DoubleValue _) -> Prelude.error "loadValue returned double when the type was specified as integer" -- this state should never be reached. I feel, therefore, that it is cleaner to simply generate an error and crash the program (in this scenario that should never happen, not even as part of exceptional behaviour) rather than introduce a whole new type constructor
                Right genError -> Right genError
    where
        generateError genError = Right $ PLYFileGenerationError (Just lineNumber) genError

-- |Load a 'Value', its type determined by the given 'HeaderValue', from the given 'LBS.ByteString'. A line number must also be provided for error messages.
loadValue :: Int -> HeaderValue -> ByteString -> Either Value PLYFileGenerationError
loadValue lineNumber HeaderDoubleValue byteString = case (readSigned readDecimal) byteString of
    Just (value, remainingByteString) -> case Data.ByteString.null remainingByteString of
        True -> Left $ DoubleValue value
        False -> Right $ PLYFileGenerationError (Just lineNumber) $ ValueUnparseable $ "Bytes were left over after parsing double value: " ++ show byteString ++ " -> " ++ show remainingByteString
    Nothing -> Right $ PLYFileGenerationError (Just lineNumber) $ ValueUnparseable "Couldn't parse value"

loadValue lineNumber HeaderIntegerValue byteString = case (readInteger byteString) of
    Just (value, remainingByteString) -> case Data.ByteString.null remainingByteString of
        True -> Left $ IntegerValue value
        False -> Right $ PLYFileGenerationError (Just lineNumber) $ ValueUnparseable $ "Bytes were left over after parsing integer value: " ++ show byteString ++ " -> " ++ show remainingByteString
    Nothing -> Right $ PLYFileGenerationError (Just lineNumber) $ ValueUnparseable "Couldn't parse value"

-- |Access a 'Double' property from an 'Element'. Returns 'Nothing' if the property is not a property containing a single real number.
getDoubleFromElement :: ByteString -> Element -> Maybe Double
getDoubleFromElement name (Element propertyMap) = do
    property <- Data.Map.lookup name propertyMap
    case property of
        ValueProperty value -> case value of
            DoubleValue x -> return x
            IntegerValue _ -> Nothing
        ListProperty _ -> Nothing

-- |Access an 'Integer' property from an 'Element'. Returns 'Nothing' if the property is not a property containing a single integer.
getIntegerFromElement :: ByteString -> Element -> Maybe Integer
getIntegerFromElement name (Element propertyMap) = do
    property <- Data.Map.lookup name propertyMap
    case property of
        ValueProperty value -> case value of
            IntegerValue x -> return x
            DoubleValue _ -> Nothing
        ListProperty _ -> Nothing

-- |Access a list of 'Double' property from an 'Element'. Strips all values from the returned list that aren't contained in a 'DoubleValue'. Returns 'Nothing' if the property is not a property containing a list of values.
getDoubleListFromElement :: ByteString -> Element -> Maybe [Double]
getDoubleListFromElement name (Element propertyMap) = do
    property <- Data.Map.lookup name propertyMap
    case property of
        ValueProperty _ -> Nothing
        ListProperty values -> return $ mapMaybe (\value -> case value of
            DoubleValue x -> Just x
            IntegerValue _ -> Nothing) values

-- |Access a list of 'Integer' property from an 'Element'. Strips all values from the returned list that aren't contained in an 'IntegerValue'. Returns 'Nothing' if the property is not a property containing a list of values.
getIntegerListFromElement :: ByteString -> Element -> Maybe [Integer]
getIntegerListFromElement name (Element propertyMap) = do
    property <- Data.Map.lookup name propertyMap
    case property of
        ValueProperty _ -> Nothing
        ListProperty values -> return $ mapMaybe (\value -> case value of
            DoubleValue _ -> Nothing
            IntegerValue x -> Just x) values