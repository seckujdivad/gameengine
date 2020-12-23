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

	enum class Interpolation
	{
		None,
		Bezier
	};

private:
	std::vector<std::vector<ControlPoint>> m_control_points;

	Interpolation m_interp_mode;

protected:
	std::vector<double> GetPrimitivesWithoutCache() const override;

public:
	std::size_t GetPrimitivesNumVertices() const override;
	Geometry::PrimitiveType GetPrimitiveType() const override;
	std::size_t GetPrimitiveSize() const override;

	void SetControlPoints(std::vector<std::vector<ControlPoint>> control_points);

	void SetInterpolation(Interpolation mode);
	Interpolation GetInterpolation() const;

	glm::ivec2 GetDimensions() const;
};