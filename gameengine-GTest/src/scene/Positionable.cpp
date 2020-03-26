#include "pch.h"
#include "scene/Positionable.h"

TEST(PositionableTests, Position_SetGet_Individual)
{
    Positionable* positionable = new Positionable();

    positionable->SetPosition(0, 90);
    positionable->SetPosition(1, 5231);
    positionable->SetPosition(2, -19);

    EXPECT_EQ(positionable->GetPosition(0), 90);
    EXPECT_EQ(positionable->GetPosition(1), 5231);
    EXPECT_EQ(positionable->GetPosition(2), -19);
}

TEST(PositionableTests, Position_SetGet_Group)
{
    Positionable* positionable = new Positionable();

    positionable->SetPosition(52, 89, -1);

    EXPECT_EQ(positionable->GetPosition(0), 52);
    EXPECT_EQ(positionable->GetPosition(1), 89);
    EXPECT_EQ(positionable->GetPosition(2), -1);
}

TEST(PositionableTests, Position_SetGet_Array)
{
    Positionable* positionable = new Positionable();

    std::array<GLfloat, 3> position_testdata;
    position_testdata.at(0) = 43;
    position_testdata.at(1) = 0;
    position_testdata.at(2) = -3;

    positionable->SetPosition(0, 43);
    positionable->SetPosition(1, 0);
    positionable->SetPosition(2, -3);

    std::array<GLfloat, 3> position = positionable->GetPosition();
    EXPECT_EQ(position_testdata, position);
}