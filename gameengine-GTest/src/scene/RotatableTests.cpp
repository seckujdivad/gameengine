#include "pch.h"
#include "scene/Rotatable.h"

TEST(RotatableTests, Rotation_SetGet_Individual)
{
    Rotatable* rotatable = new Rotatable();

    rotatable->SetRotation(0, 90);
    rotatable->SetRotation(1, 5231);
    rotatable->SetRotation(2, -19);

    EXPECT_EQ(rotatable->GetRotation(0), 90);
    EXPECT_EQ(rotatable->GetRotation(1), 5231);
    EXPECT_EQ(rotatable->GetRotation(2), -19);
}

TEST(RotatableTests, Rotation_SetGet_Group)
{
    Rotatable* rotatable = new Rotatable();

    rotatable->SetRotation(52, 89, -1);

    EXPECT_EQ(rotatable->GetRotation(0), 52);
    EXPECT_EQ(rotatable->GetRotation(1), 89);
    EXPECT_EQ(rotatable->GetRotation(2), -1);
}

TEST(RotatableTests, Rotation_SetGet_Array)
{
    Rotatable* rotatable = new Rotatable();

    std::array<GLfloat, 3> rotation_testdata;
    rotation_testdata.at(0) = 43;
    rotation_testdata.at(1) = 0;
    rotation_testdata.at(2) = -3;

    rotatable->SetRotation(0, 43);
    rotatable->SetRotation(1, 0);
    rotatable->SetRotation(2, -3);

    std::array<GLfloat, 3> rotation = rotatable->GetRotation();
    EXPECT_EQ(rotation_testdata, rotation);
}