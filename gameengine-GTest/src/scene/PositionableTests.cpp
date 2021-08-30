#include "pch.h"
#include "scene/transformations/Positionable.h"

TEST(PositionableTests, Position_SetGet)
{
	Positionable* positionable = new Positionable();

	positionable->SetPosition(glm::dvec3(52.0, 89.0, -1.0));

	EXPECT_EQ(positionable->GetPosition(), glm::dvec3(52.0, 89.0, -1.0));

	delete positionable;
}