#include "pch.h"
#include "scene/Camera.h"

TEST(CameraTests, FOV_SetGet)
{
	Camera cam;
	EXPECT_EQ(cam.GetFOV(), 45);

	cam.SetFOV(90);
	EXPECT_EQ(cam.GetFOV(), 90);
}

TEST(CameraTests, Clips_SetGet)
{
	std::tuple<double, double> clips = { 10.0, 50.0 };

	Camera cam;
	cam.SetClips(clips);
	EXPECT_EQ(cam.GetClips(), clips);
}

TEST(CameraTests, ViewportDimensions_SetGet)
{
	std::tuple<int, int> dimensions = { 1920, 1080 };

	Camera cam;
	cam.SetViewportDimensions(dimensions);
	EXPECT_EQ(cam.GetViewportDimensions(), dimensions);
}

TEST(CameraTests, MatrixGeneration)
{
	Camera cam;
	cam.SetFOV(90.0);
	cam.SetPosition(glm::dvec3(10.0, -1.0, 5.0));
	cam.SetRotation(glm::dvec3(90.0, 90.0, 90.0));
	cam.SetClips(std::tuple<double, double>(0.1, 50.0));
	cam.SetViewportDimensions(std::tuple<int, int>(1920, 1080));

	glm::dmat4 reference_persp = glm::perspective(glm::radians(90.0), 1920.0 / 1080.0, 0.1, 50.0);
	glm::dmat4 reference_combined = reference_persp * cam.GetRotationMatrixInverse() * cam.GetTranslationMatrixInverse();

	glm::dmat4 generated_persp = cam.GetPerspectiveMatrix();
	glm::dmat4 generated_combined = cam.GetCombinedMatrix();

	EXPECT_EQ(reference_persp, generated_persp);
	EXPECT_EQ(reference_combined, generated_combined);
}