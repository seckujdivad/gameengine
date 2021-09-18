module GameEngineServer.State.Scene.Motion (Motion (..), mtnVelocity) where

import Linear.V3 (V3)


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