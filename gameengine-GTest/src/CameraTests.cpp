#include "pch.h"
#include "Camera.h"

TEST(CameraTests, Identifier_SetGet) {
    Camera* cam = new Camera();
    EXPECT_EQ(cam->GetIdentifier(), "");

    cam->SetIdentifier("Hello World!");
    EXPECT_EQ(cam->GetIdentifier(), "Hello World!");
}

TEST(CameraTests, FOV_SetGet) {
    Camera* cam = new Camera();
    EXPECT_EQ(cam->GetFOV(), 45);

    cam->SetFOV(90);
    EXPECT_EQ(cam->GetFOV(), 90);
}

TEST(CameraTests, Position_SetGet_Individual) {
    Camera* cam = new Camera();

    cam->SetPosition(0, 90);
    cam->SetPosition(1, 5231);
    cam->SetPosition(2, -19);

    EXPECT_EQ(cam->GetPosition(0), 90);
    EXPECT_EQ(cam->GetPosition(1), 5231);
    EXPECT_EQ(cam->GetPosition(2), -19);
}

TEST(CameraTests, Position_SetGet_Group) {
    Camera* cam = new Camera();

    cam->SetPosition(52, 89, -1);

    EXPECT_EQ(cam->GetPosition(0), 52);
    EXPECT_EQ(cam->GetPosition(1), 89);
    EXPECT_EQ(cam->GetPosition(2), -1);
}

TEST(CameraTests, Position_SetGet_Array) {
    Camera* cam = new Camera();

    std::array<GLfloat, 3> position_testdata;
    position_testdata.at(0) = 43;
    position_testdata.at(1) = 0;
    position_testdata.at(2) = -3;

    cam->SetPosition(0, 43);
    cam->SetPosition(1, 0);
    cam->SetPosition(2, -3);

    std::array<GLfloat, 3> position = cam->GetPosition();
    EXPECT_EQ(position_testdata, position);
}

TEST(CameraTests, Rotation_SetGet_Individual) {
    Camera* cam = new Camera();

    cam->SetRotation(0, 90);
    cam->SetRotation(1, 5231);
    cam->SetRotation(2, -19);

    EXPECT_EQ(cam->GetRotation(0), 90);
    EXPECT_EQ(cam->GetRotation(1), 5231);
    EXPECT_EQ(cam->GetRotation(2), -19);
}

TEST(CameraTests, Rotation_SetGet_Group) {
    Camera* cam = new Camera();

    cam->SetRotation(52, 89, -1);

    EXPECT_EQ(cam->GetRotation(0), 52);
    EXPECT_EQ(cam->GetRotation(1), 89);
    EXPECT_EQ(cam->GetRotation(2), -1);
}

TEST(CameraTests, Rotation_SetGet_Array) {
    Camera* cam = new Camera();

    std::array<GLfloat, 3> rotation_testdata;
    rotation_testdata.at(0) = 43;
    rotation_testdata.at(1) = 0;
    rotation_testdata.at(2) = -3;

    cam->SetRotation(0, 43);
    cam->SetRotation(1, 0);
    cam->SetRotation(2, -3);

    std::array<GLfloat, 3> rotation = cam->GetRotation();
    EXPECT_EQ(rotation_testdata, rotation);
}