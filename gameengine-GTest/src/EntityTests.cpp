#include "pch.h"
#include "Entity.h"

TEST(EntityTests, Identifier_SetGet)
{
    Entity* entity = new Entity();
    EXPECT_EQ(entity->GetIdentifier(), "");

    entity->SetIdentifier("Hello World!");
    EXPECT_EQ(entity->GetIdentifier(), "Hello World!");
}

TEST(EntityTests, Position_SetGet_Individual)
{
    Entity* entity = new Entity();

    entity->SetPosition(0, 90);
    entity->SetPosition(1, 5231);
    entity->SetPosition(2, -19);

    EXPECT_EQ(entity->GetPosition(0), 90);
    EXPECT_EQ(entity->GetPosition(1), 5231);
    EXPECT_EQ(entity->GetPosition(2), -19);
}

TEST(EntityTests, Position_SetGet_Group)
{
    Entity* entity = new Entity();

    entity->SetPosition(52, 89, -1);

    EXPECT_EQ(entity->GetPosition(0), 52);
    EXPECT_EQ(entity->GetPosition(1), 89);
    EXPECT_EQ(entity->GetPosition(2), -1);
}

TEST(EntityTests, Position_SetGet_Array)
{
    Entity* entity = new Entity();

    std::array<GLfloat, 3> position_testdata;
    position_testdata.at(0) = 43;
    position_testdata.at(1) = 0;
    position_testdata.at(2) = -3;

    entity->SetPosition(0, 43);
    entity->SetPosition(1, 0);
    entity->SetPosition(2, -3);

    std::array<GLfloat, 3> position = entity->GetPosition();
    EXPECT_EQ(position_testdata, position);
}

TEST(EntityTests, Rotation_SetGet_Individual)
{
    Entity* entity = new Entity();

    entity->SetRotation(0, 90);
    entity->SetRotation(1, 5231);
    entity->SetRotation(2, -19);

    EXPECT_EQ(entity->GetRotation(0), 90);
    EXPECT_EQ(entity->GetRotation(1), 5231);
    EXPECT_EQ(entity->GetRotation(2), -19);
}

TEST(EntityTests, Rotation_SetGet_Group)
{
    Entity* entity = new Entity();

    entity->SetRotation(52, 89, -1);

    EXPECT_EQ(entity->GetRotation(0), 52);
    EXPECT_EQ(entity->GetRotation(1), 89);
    EXPECT_EQ(entity->GetRotation(2), -1);
}

TEST(EntityTests, Rotation_SetGet_Array)
{
    Entity* entity = new Entity();

    std::array<GLfloat, 3> rotation_testdata;
    rotation_testdata.at(0) = 43;
    rotation_testdata.at(1) = 0;
    rotation_testdata.at(2) = -3;

    entity->SetRotation(0, 43);
    entity->SetRotation(1, 0);
    entity->SetRotation(2, -3);

    std::array<GLfloat, 3> rotation = entity->GetRotation();
    EXPECT_EQ(rotation_testdata, rotation);
}