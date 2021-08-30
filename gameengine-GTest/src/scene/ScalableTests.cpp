#include "pch.h"
#include "scene/transformations/Scalable.h"

TEST(ScalableTests, Scale_SetGet)
{
	Scalable* scalable = new Scalable();

	scalable->SetScale(glm::dvec3(52.0, 89.0, -1.0));

	EXPECT_EQ(scalable->GetScale(), glm::dvec3(52.0, 89.0, -1.0));

	delete scalable;
}