#include "pch.h"
#include "scene/light/PointLight.h"

TEST(PointLightTests, Intensity_SetGet)
{
	glm::vec3 intensity = glm::vec3(0.1f, 0.9f, 0.4f);
	PointLight light(-1);
	light.SetIntensity(intensity);
	EXPECT_EQ(light.GetIntensity(), intensity);
}

TEST(PointLightTests, ShadowBias_SetGet)
{
	double bias = -0.5;
	PointLight light(-1);
	light.SetShadowBias(bias);
	EXPECT_EQ(light.GetShadowBias(), bias);
}