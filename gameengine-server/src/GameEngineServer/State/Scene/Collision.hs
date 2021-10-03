module GameEngineServer.State.Scene.Collision (Collision (..), collide) where

import Linear.V3 (V3 (V3))
import Linear.Metric (distance)

import GameEngineServer.State.Scene.Motion (Motion (..), motionFromVelocityAndMomentOrMass, motionFromMomentumAndMomentOrMass)


-- |Describes the motion of two particles (which are nominally involved in some kind of collision)
type MotionPair a = (Motion a, Motion a)

-- |Describes a Newtonian collision between two particles
data Collision a =
    -- |Describes a Newtonian collision between two particles
    Collision {
        -- |Motion of the particles
        collMotion :: MotionPair a,
        -- |Coefficient of restitution of the collision
        collRestitutionCoefficient :: a
    }

-- |Process a collision between two objects
collide :: (Num a, Floating a) => Collision a -> (V3 a, V3 a) -> MotionPair a
collide (Collision (firstMotion, secondMotion) restitutionCoefficient) (_, secondResultantUnitDirection) = (resultantFirstMotion, resultantSecondMotion)
    where
        -- momentum and kinetic energy are conserved (minus the coefficient of restitution)
        resultantMomentum = mtnMomentum firstMotion + mtnMomentum secondMotion
        resultantKEnergy = restitutionCoefficient * (mtnKEnergy firstMotion + mtnKEnergy secondMotion)

        absoluteResultantMomentum = distance (V3 0 0 0) resultantMomentum

        -- solve second particle absolute resultant velocity as part of the system of simultaneous equations
        -- WARNING - likely failure point
        secondAbsoluteResultantVelocityNumeratorConstant = 2 * (mtnMomentOrMass secondMotion) * absoluteResultantMomentum
        secondAbsoluteResultantVelocityNumeratorIrrational = 2 * mtnMomentOrMass firstMotion * mtnMomentOrMass secondMotion * ((2 * absoluteResultantMomentum * absoluteResultantMomentum) - (resultantKEnergy * (mtnMomentOrMass firstMotion + mtnMomentOrMass secondMotion)))

        secondAbsoluteResultantVelocityDenominator = 2 * (mtnMomentOrMass secondMotion) * ((mtnMomentOrMass firstMotion) + (mtnMomentOrMass secondMotion))

        secondAbsoluteResultantVelocity = (secondAbsoluteResultantVelocityNumeratorConstant + (sqrt secondAbsoluteResultantVelocityNumeratorIrrational)) / secondAbsoluteResultantVelocityDenominator

        secondResultantVelocity = (return secondAbsoluteResultantVelocity) * secondResultantUnitDirection

        -- solve rest of second particle
        secondResultantMomentum = (return $ mtnMomentOrMass secondMotion) * secondResultantVelocity

        resultantSecondMotion = motionFromVelocityAndMomentOrMass secondResultantVelocity (mtnMomentOrMass secondMotion)

        -- find first particle from second particle
        firstResultantMomentum = resultantMomentum - secondResultantMomentum
        resultantFirstMotion = motionFromMomentumAndMomentOrMass firstResultantMomentum (mtnMomentOrMass firstMotion)