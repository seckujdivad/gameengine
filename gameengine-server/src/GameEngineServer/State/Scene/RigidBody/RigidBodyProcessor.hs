module GameEngineServer.State.Scene.RigidBody.RigidBodyProcessor (processRigidBodies) where

import Data.Fixed (Pico)

import Data.Map.Strict (Map, keys, lookup)

import Data.Text (Text)

import GameEngineServer.State.Scene.Scene (Scene (..))
import GameEngineServer.State.Scene.Model.Model (Model)


-- |Process all rigid bodies in a 'Scene'
processRigidBodies :: Pico -> Scene -> Scene
processRigidBodies timestep scene = scene {scnModels = processModels timestep (scnModels scene)}

processModels :: Pico -> Map Text Model -> Map Text Model
processModels timestep models = foldr (processModel timestep) models (keys models)

processModel :: Pico -> Text -> Map Text Model -> Map Text Model 
processModel timestep modelName models = case Data.Map.Strict.lookup modelName models of
    Just model -> models
    Nothing -> models

{-
trace model forward to time step
find first collision in that path
if collision exists:
    process collision
    go to top - trace from time of collision to end of time step

two key algorithms: collision tracer and collision processor
-}