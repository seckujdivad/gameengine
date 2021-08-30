#include "pch.h"
#include "scene/transformations/Rotatable.h"

TEST(RotatableTests, Rotation_SetGet)
{
	Rotatable* rotatable = new Rotatable();

	rotatable->SetRotation(glm::dvec3(52.0, 89.0, -1.0));

	EXPECT_EQ(rotatable->GetRotation(), glm::dvec3(52.0, 89.0, -1.0));

	delete rotatable;
}