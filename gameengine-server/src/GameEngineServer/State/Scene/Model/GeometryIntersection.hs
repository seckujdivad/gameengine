module GameEngineServer.State.Scene.Model.GeometryIntersection (findIntersection) where

import Linear.V3 (V3)

import GameEngineServer.State.Scene.Model.Geometry (Geometry (..), Face (..))


-- |Find where (if ever) two pieces of 'Geometry' are intersecting
findIntersection :: Geometry -> Geometry -> [V3 Double]
findIntersection (Polygonal firstFaces) (Polygonal secondFaces) = firstVerticesInSecondPolygonal ++ secondVerticesInFirstPolygonal -- do heuristic check first
    where
        firstVertices = concat $ map faceVertices firstFaces
        secondVertices = concat $ map faceVertices secondFaces

        firstVerticesInSecondPolygonal = filter (pointIsInsideGeometry (Polygonal secondFaces)) firstVertices
        secondVerticesInFirstPolygonal = filter (pointIsInsideGeometry (Polygonal firstFaces)) secondVertices

pointIsInsideGeometry :: Geometry -> V3 Double -> Bool
pointIsInsideGeometry (Polygonal faces) point = False -- trace intersections from point to arbitrarily far away point