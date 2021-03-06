#pragma once

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <tuple>

#include "transformations/LocallyMovable.h"
#include "Nameable.h"
#include "Referenceable.h"

class Camera : public LocallyMovable, public Nameable
{
private:
	double m_fov = 45.0;
	std::tuple<double, double> m_clips = std::tuple<double, double>(0.1, 100.0);
	std::tuple<int, int> m_viewport_dimensions = std::tuple<int, int>(1, 1);
	glm::ivec2 m_ssr_region_dimensions = glm::ivec2(1, 1);

public:
	Camera();

	void SetFOV(double fov);
	double GetFOV() const;

	void SetClips(std::tuple<double, double> clips);
	std::tuple<double, double> GetClips() const;

	void SetViewportDimensions(std::tuple<int, int> dimensions);
	std::tuple<int, int> GetViewportDimensions() const;

	glm::dmat4 GetPerspectiveMatrix() const;
	glm::dmat4 GetCombinedMatrix() const;

	void SetSSRRegionDimensions(glm::ivec2 dimensions);
	glm::ivec2 GetSSRRegionDimensions() const;
};