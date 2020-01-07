#include "pch.h"
#include "Camera.h"


TEST(CameraTests, SetGet) {
    Camera* cam = new Camera();
    EXPECT_EQ(cam->GetIdentifier(), "");

    cam->SetIdentifier("Hello World!");
    EXPECT_EQ(cam->GetIdentifier(), "Hello World!");
}