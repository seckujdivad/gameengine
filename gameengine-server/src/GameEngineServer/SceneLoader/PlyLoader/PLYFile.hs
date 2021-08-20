module GameEngineServer.SceneLoader.PlyLoader.PLYFile (PLYFile (..), Element (..), Property (..), Value (..), emptyPLYFile) where

import Data.ByteString (ByteString)

import Data.Map (Map, empty)


-- |Logically represents the structure of a .PLY file. Maps the names of elements onto lists of those elements.
newtype PLYFile =
    -- |Logically represents the structure of a .PLY file. Maps the names of elements onto lists of those elements.
    PLYFile {
        -- |Maps the names of elements onto lists of those elements
        plyfElements :: Map ByteString [Element]
    }

instance Show PLYFile where
    show (PLYFile elementsMap) = show elementsMap

-- |An element in a .PLY file. Logically is the child of a 'PLYFile'. Maps the names of properties onto properties which store their values.
newtype Element =
    -- |An element in a .PLY file. Logically is the child of a 'PLYFile'. Maps the names of properties onto properties which store their values.
    Element (Map ByteString Property)

instance Show Element where
    show (Element elements) = show elements

-- |A property in a .PLY file. Logically is the child of an 'Element'. Stores either a single 'Value' or a list of 'Value's.
data Property =
    -- |A property in a .PLY file. Logically is the child of an 'Element'. Stores a single 'Value'.
    ValueProperty Value
    -- |A property in a .PLY file. Logically is the child of an 'Element'. Stores a list of 'Value's.
    | ListProperty [Value]

instance Show Property where
    show (ValueProperty value) = "Value: " ++ show value
    show (ListProperty values) = "Values: " ++ show values

-- |A value in a .PLY file. Logically is the child of a 'Property'. Stores either a 'Double' or an 'Integer'
data Value =
    -- |A value in a .PLY file. Logically is the child of a 'Property'. Stores a 'Double'.
    DoubleValue Double
    -- |A value in a .PLY file. Logically is the child of a 'Property'. Stores an 'Integer'.
    | IntegerValue Integer

instance Show Value where
    show (DoubleValue number) = "(real) " ++ show number
    show (IntegerValue number) = "(integer) " ++ show number

-- |Represents the base state of a 'PLYFile'. Contains no elements.
emptyPLYFile :: PLYFile
emptyPLYFile = PLYFile empty