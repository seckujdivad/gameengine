#pragma once

#include <vector>

#include <glm/glm.hpp>

#include "Geometry.h"

class Patch : public Geometry
{
public:
	struct ControlPoint
	{
		ControlPoint(glm::dvec3 vertex = glm::dvec3(0.0), glm::dvec2 uv = glm::dvec2(0.0));

		glm::dvec3 vertex;
		glm::dvec2 uv;
	};

private:
	std::vector<std::vector<ControlPoint>> m_control_points;

protected:
	std::vector<double> GetPrimitivesWithoutCache() const override;

public:
	std::size_t GetPrimitivesNumValues() const override;
	Geometry::PrimitiveType GetPrimitiveType() const override;

	void SetControlPoints(std::vector<std::vector<ControlPoint>> control_points);
};