#include "pch.h"
#include "scene/Scalable.h"

TEST(ScalableTests, Scale_SetGet_Individual)
{
    Scalable* scalable = new Scalable();

    scalable->SetScale(0, 90);
    scalable->SetScale(1, 5231);
    scalable->SetScale(2, -19);

    EXPECT_EQ(scalable->GetScale(0), 90);
    EXPECT_EQ(scalable->GetScale(1), 5231);
    EXPECT_EQ(scalable->GetScale(2), -19);
}

TEST(ScalableTests, Scale_SetGet_Group)
{
    Scalable* scalable = new Scalable();

    scalable->SetScale(52, 89, -1);

    EXPECT_EQ(scalable->GetScale(0), 52);
    EXPECT_EQ(scalable->GetScale(1), 89);
    EXPECT_EQ(scalable->GetScale(2), -1);
}

TEST(ScalableTests, Scale_SetGet_Array)
{
    Scalable* scalable = new Scalable();

    std::array<GLfloat, 3> scale_testdata;
    scale_testdata.at(0) = 43;
    scale_testdata.at(1) = 0;
    scale_testdata.at(2) = -3;

    scalable->SetScale(0, 43);
    scalable->SetScale(1, 0);
    scalable->SetScale(2, -3);

    std::array<GLfloat, 3> scale = scalable->GetScale();
    EXPECT_EQ(scale_testdata, scale);
}