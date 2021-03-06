#include "pch.h"
#include "scene/transformations/Scalable.h"

TEST(ScalableTests, Scale_SetGet_Individual)
{
	Scalable* scalable = new Scalable();

	scalable->SetScale(0, 90);
	scalable->SetScale(1, 5231);
	scalable->SetScale(2, -19);

	EXPECT_EQ(scalable->GetScale(0), 90);
	EXPECT_EQ(scalable->GetScale(1), 5231);
	EXPECT_EQ(scalable->GetScale(2), -19);

	delete scalable;
}

TEST(ScalableTests, Scale_SetGet_Group)
{
	Scalable* scalable = new Scalable();

	scalable->SetScale(52, 89, -1);

	EXPECT_EQ(scalable->GetScale(0), 52);
	EXPECT_EQ(scalable->GetScale(1), 89);
	EXPECT_EQ(scalable->GetScale(2), -1);

	delete scalable;
}

TEST(ScalableTests, Scale_SetGet_Vector)
{
	Scalable* scalable = new Scalable();

	glm::dvec3 scale_testdata = glm::dvec3(43, 0, -3);

	scalable->SetScale(0, 43);
	scalable->SetScale(1, 0);
	scalable->SetScale(2, -3);

	glm::dvec3 scale = scalable->GetScale();
	EXPECT_EQ(scale_testdata, scale);

	delete scalable;
}