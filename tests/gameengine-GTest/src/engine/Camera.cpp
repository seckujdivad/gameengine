#include "pch.h"
//#include "engine/Camera.h"
#include <engine/Camera.h>

namespace CameraTests
{
	TEST(Identifier, SetGet)
	{
		Camera* cam = new Camera();
		

		cam->SetIdentifier("Test identifier");
		EXPECT_EQ(cam->GetIdentifier(), "Test identifier");
	}
}