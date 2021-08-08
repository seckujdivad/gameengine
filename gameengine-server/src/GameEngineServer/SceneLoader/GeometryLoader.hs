{-# LANGUAGE OverloadedStrings #-}

module GameEngineServer.SceneLoader.GeometryLoader () where

import qualified Data.Map as Map

import qualified Data.HashMap.Strict as HashMap

import Data.Aeson (FromJSON (..), (.:), (.:?), withObject, Object, Value)
import Data.Aeson.Types (Parser)

import Data.Text (Text)

import System.FilePath (FilePath)

import Linear.V3 (V3 (..))

import Data.Maybe (fromMaybe)

import GameEngineServer.State.Scene.Model.Geometry (Geometry (..))
import GameEngineServer.SceneLoader.VectorLoader (V3)


-- |Lookup table for mapping geometry names (found in scene files) to 'GeometryInfo' to be used for loading
data GeometryTable =
    -- |Lookup table for mapping geometry names (found in scene files) to 'GeometryInfo' to be used for loading
    GeometryTable (Map.Map Text [GeometryInfo])

-- |Stores information to allow another function to load 'Geometry'
data GeometryInfo =
    -- |Information on how to load 'Polygonal' geometry
    PLYGeometry {
        plygPath :: FilePath,
        plygInvertNormals :: Bool,
        plygGrid :: Maybe (V3 Double),
        plygMerge :: Maybe Double
    }

instance FromJSON GeometryTable where
    parseJSON = withObject "GeometryTable" $ \obj -> do
        plysMaybe <- obj .:? "ply"
        geometryTable <- case plysMaybe of
            Just plys -> withObject "GeometryTable PLYs" (\obj -> (loadPLYGeometryTable) (GeometryTable Map.empty) obj) plys
            Nothing -> return $ GeometryTable Map.empty
        
        -- TODO: load BPTs

        return geometryTable

-- |Load the "ply" section of the "models" section of a scene file
loadPLYGeometryTable :: GeometryTable -> Object -> Parser GeometryTable
loadPLYGeometryTable geometryTable obj = foldr (load) (return geometryTable) mapContents
    where
        mapContents = HashMap.toList obj

        -- |Load a single 'PLYGeometry' into the 'GeometryTable'
        load :: (Text, Value) -> Parser GeometryTable -> Parser GeometryTable
        load (key, value) geometryParser = withObject "PLY config" plyConfigParser value
            where
                plyConfigParser :: Object -> Parser GeometryTable
                plyConfigParser obj = do
                    geometryTable <- geometryParser
                    let GeometryTable geometryMap = geometryTable
                    
                    plyPath <- obj .: "path"
                    plyInvertNormalsMaybe <- obj .:? "invert normals"
                    plyGrid <- obj .:? "grid"
                    plyMergeGeometryDistance <- obj .:? "merge geometry distance"
                    
                    let
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

                    return $ GeometryTable $ Map.alter geometryInserter key geometryMap