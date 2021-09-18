module GameEngineServer.State.Scene.Collision (Collision (..), collide) where

import Linear.V3 (V3)

import GameEngineServer.State.Scene.Motion (Motion (..))


type MotionPair a = (Motion a, Motion a)

data Collision a =
    Collision {
        collMotion :: MotionPair a,
        collRestitutionCoefficient :: a
    }

-- |Process a collision between two objects
collide :: Num a => Collision a -> (V3 a, V3 a) -> MotionPair a
collide (Collision (firstMotion, secondMotion) restitutionCoefficient) (firstResultantUnitDirection, secondResultantUnitDirection) = (firstMotion, secondMotion)
    where
        resultantMomentum = mtnMomentum firstMotion + mtnMomentum secondMotion
        resultantKEnergy = restitutionCoefficient * (mtnKEnergy firstMotion + mtnKEnergy secondMotion)