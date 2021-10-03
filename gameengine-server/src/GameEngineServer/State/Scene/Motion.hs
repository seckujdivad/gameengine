module GameEngineServer.State.Scene.Motion (Motion (..), mtnVelocity, motionFromVelocityAndMomentOrMass, motionFromMomentumAndMomentOrMass) where

import Linear.V3 (V3)
import Linear.Metric (distance)


-- |Describes either translational or rotational motion
data Motion a =
    -- |Describes either translational or rotational motion
    Motion {
        -- |Kinetic energy
        mtnKEnergy :: a,
        -- |Momentum
        mtnMomentum :: V3 a,
        -- |The mass or moment of inertia of the object being described
        mtnMomentOrMass :: a
    }

-- |Get the velocity described by a 'Motion'
mtnVelocity :: Fractional a => Motion a -> V3 a
mtnVelocity motion = (mtnMomentum motion) / (return $ mtnMomentOrMass motion)

instance Show a => Show (Motion a) where
    show (Motion kinEnergy momentum momentOrMass) = "Motion - kinetic energy: " ++ show kinEnergy ++ ", momentum: " ++ show momentum ++ ", moment of inertia/mass: " ++ show momentOrMass

-- |Construct a 'Motion' from a given velocity and moment of inertia/mass
motionFromVelocityAndMomentOrMass :: Floating a => V3 a -> a -> Motion a
motionFromVelocityAndMomentOrMass velocity momentOrMass = Motion kineticEnergy momentum momentOrMass
    where
        velocityScalar = distance (return 0) velocity
        kineticEnergy = 0.5 * momentOrMass * velocityScalar * velocityScalar
        momentum = velocity * (return momentOrMass)

-- |Construct a 'Motion' from a given momentum and moment of inertia/mass
motionFromMomentumAndMomentOrMass :: Floating a => V3 a -> a -> Motion a
motionFromMomentumAndMomentOrMass momentum momentOrMass = Motion kineticEnergy momentum momentOrMass
    where
        velocityScalar = (distance (return 0) momentum) / momentOrMass
        kineticEnergy = 0.5 * momentOrMass * velocityScalar * velocityScalar