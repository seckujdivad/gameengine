#include "Patch.h"

#include <stdexcept>

std::vector<double> Patch::GetPrimitivesWithoutCache() const
{
	return std::vector<double>(); //TODO: implement patch construction from stored data
}

std::size_t Patch::GetPrimitivesNumValues() const
{
	return this->m_control_points.size() * this->m_control_points.at(0).size() * GAMEENGINE_VALUES_PER_VERTEX;
}

Geometry::PrimitiveType Patch::GetPrimitiveType() const
{
	return Geometry::PrimitiveType::Patch;
}

void Patch::SetControlPoints(std::vector<std::vector<ControlPoint>> control_points)
{
	if (control_points.size() < 2)
	{
		throw std::invalid_argument("Patches must be at least 2x2");
	}
	else
	{
		std::size_t line_length = 0;
		for (const std::vector<ControlPoint>& cp_line : control_points)
		{
			if (line_length == 0)
			{
				line_length = cp_line.size();
			}
			else if (line_length != cp_line.size())
			{
				throw std::invalid_argument("All control point lines must be the same length");
			}
		}

		if (line_length <= 2)
		{
			throw std::invalid_argument("Patches must be at least 2x2");
		}

		this->m_control_points = control_points;
		this->InvalidatePrimitivesCache();
	}
}

Patch::ControlPoint::ControlPoint(glm::dvec3 vertex, glm::dvec2 uv) : vertex(vertex), uv(uv)
{
}
