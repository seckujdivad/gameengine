{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.GeometryLoader (GeometryInfoTable (..), GeometryInfo (..), createGeometryTable, GeometryTable (..)) where

import Prelude hiding (readFile)

import qualified Data.Map as Map

import qualified Data.HashMap.Strict as HashMap

import Data.Aeson (FromJSON (..), (.:), (.:?), withObject, Object, Value)
import Data.Aeson.Types (Parser)

import Data.Text (Text)

import System.FilePath ((</>))

import Linear.V3 (V3 (..))

import Data.Maybe (fromMaybe, mapMaybe)

import Data.ByteString.Lazy (readFile)

import GameEngineServer.State.Scene.Model.Geometry (Geometry (..), invertNormals, snapVerticesToGrid, mergeVertices)
import GameEngineServer.SceneLoader.VectorLoader (extractJSONableV3)
import GameEngineServer.SceneLoader.PlyLoader.PlyLoader (loadPolygonalFromPLY)


-- |Lookup table for mapping geometry names (found in scene files) to 'GeometryInfo' to be used for loading
data GeometryInfoTable =
    -- |Lookup table for mapping geometry names (found in scene files) to 'GeometryInfo' to be used for loading
    GeometryInfoTable (Map.Map Text [GeometryInfo])

-- |Stores information to allow another function to load 'Geometry'
data GeometryInfo =
    -- |Information on how to load 'Polygonal' geometry
    PLYGeometry {
        plygPath :: FilePath,
        plygInvertNormals :: Bool,
        plygGrid :: Maybe (V3 Double),
        plygMerge :: Maybe Double
    }

instance FromJSON GeometryInfoTable where
    parseJSON = withObject "GeometryTable" $ \obj -> do
        plysMaybe <- obj .:? "ply"
        geometryTable <- case plysMaybe of
            Just plys -> withObject "GeometryTable PLYs" (\plyObj -> loadPLYGeometryTable plyObj) plys
            Nothing -> return $ GeometryInfoTable Map.empty
        
        -- TODO: load BPTs

        return geometryTable

-- |Load the "ply" section of the "models" section of a scene file
loadPLYGeometryTable :: Object -> Parser GeometryInfoTable
loadPLYGeometryTable obj = foldr (load) (return $ GeometryInfoTable Map.empty) mapContents
    where
        mapContents = HashMap.toList obj

        -- |Load a single 'PLYGeometry' into the 'GeometryTable'
        load :: (Text, Value) -> Parser GeometryInfoTable -> Parser GeometryInfoTable
        load (key, value) geometryParser = withObject "PLY config" plyConfigParser value
            where
                plyConfigParser :: Object -> Parser GeometryInfoTable
                plyConfigParser plyJSONObj = do
                    geometryInfoTable <- geometryParser
                    let GeometryInfoTable geometryMap = geometryInfoTable
                    
                    plyPath <- plyJSONObj .: "path"
                    plyInvertNormalsMaybe <- plyJSONObj .:? "invert normals"
                    plyGridWrapped <- plyJSONObj .:? "grid"
                    plyMergeGeometryDistance <- plyJSONObj .:? "merge geometry distance"

                    let
                        plyGrid = case plyGridWrapped of
                            Just vector -> Just $ extractJSONableV3 vector
                            Nothing -> Nothing
                        
                        geometry = PLYGeometry {
                            plygPath = plyPath,
                            plygInvertNormals = fromMaybe False plyInvertNormalsMaybe,
                            plygGrid = plyGrid,
                            plygMerge = plyMergeGeometryDistance
                        }

                        -- |Insert the 'PLYGeometry' into the 'Map.Map'
                        geometryInserter :: Maybe [GeometryInfo] -> Maybe [GeometryInfo]
                        geometryInserter geometriesMaybe = case geometriesMaybe of
                            Just geometries -> Just $ geometry:geometries
                            Nothing -> Just [geometry]

                    return $ GeometryInfoTable $ Map.alter geometryInserter key geometryMap

-- |Lookup table for mapping geometry names onto 'Geometry'
data GeometryTable =
    -- |Lookup table for mapping geometry names onto 'Geometry'
    GeometryTable (Map.Map Text [Geometry])

-- |Construct a 'GeometryTable' from a 'GeometryInfoTable' and the root 'FilePath'
createGeometryTable :: GeometryInfoTable -> FilePath -> IO GeometryTable
createGeometryTable (GeometryInfoTable infoMap) rootPath = do
    let
        geometryMapMultipleIO = fmap (map (loadGeometry rootPath)) infoMap
        geometryMapIOValues = fmap sequence geometryMapMultipleIO

        translationFunc :: [(Text, IO [Maybe Geometry])] -> [(Text, [Geometry])] -> IO [(Text, [Geometry])]
        translationFunc [] geometryMapListIO = return geometryMapListIO
        translationFunc ((key, geometryListIO):remaining) geometryMapList = do
            geometryList <- geometryListIO
            let newGeometryMapList = (key, mapMaybe id geometryList):geometryMapList
            finalGeometryMapList <- translationFunc remaining newGeometryMapList
            return $ finalGeometryMapList
    
    geometryMapList <- translationFunc (Map.toList geometryMapIOValues) []
    
    return $ GeometryTable (Map.fromList geometryMapList)

-- |Loads 'Geometry' from a root 'FilePath' and 'GeometryInfo'. When an error occurs, outputs it to console and returns 'Nothing'.
loadGeometry :: FilePath -> GeometryInfo -> IO (Maybe Geometry)
loadGeometry rootPath geometryInfo = case geometryInfo of
    PLYGeometry plyPath plyInvertNormals plyGridSizeMaybe plyMergeDistanceMaybe -> do
        let fullPath = rootPath </> plyPath
        fileContents <- readFile fullPath
        let loadedGeometryOrError = loadPolygonalFromPLY fileContents
        case loadedGeometryOrError of
            Left loadedGeometry -> return $ Just $ composedFunction loadedGeometry
                where
                    composedFunction = (if plyInvertNormals then invertNormals else id)
                        . (case plyGridSizeMaybe of
                            Just gridSize -> snapVerticesToGrid gridSize
                            Nothing -> id)
                        . (case plyMergeDistanceMaybe of
                            Just mergeDistance -> mergeVertices mergeDistance
                            Nothing -> id)

            Right loadingError -> do
                putStrLn $ fullPath ++ ": "
                print loadingError
                return Nothing