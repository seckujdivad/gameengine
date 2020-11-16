#include "pch.h"
#include "scene/Camera.h"

TEST(CameraTests, FOV_SetGet)
{
	Camera* cam = new Camera();
	EXPECT_EQ(cam->GetFOV(), 45);

	cam->SetFOV(90);
	EXPECT_EQ(cam->GetFOV(), 90);

	delete cam;
}