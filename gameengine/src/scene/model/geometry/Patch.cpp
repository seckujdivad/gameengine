#include "Patch.h"

#include <stdexcept>

std::vector<double> Patch::GetPrimitivesWithoutCache() const
{
	std::vector<double> result;

	if (this->m_interp_mode == Interpolation::None)
	{
		std::vector<glm::ivec2> indices;
		result.reserve(this->GetPrimitivesNumValues());
		indices.reserve(this->GetPrimitivesNumValues() / GAMEENGINE_VALUES_PER_VERTEX);

		for (int u = 0; u < static_cast<int>(this->m_control_points.size()) - 1; u++)
		{
			for (int v = 0; v < static_cast<int>(this->m_control_points.size()) - 1; v++)
			{
				//generate triangle from the quads, assume that the patch starts in the bottom left
				//first triangle
				indices.push_back(glm::ivec2(u, v));
				indices.push_back(glm::ivec2(u + 1, v));
				indices.push_back(glm::ivec2(u + 1, v + 1));

				//second triangle
				indices.push_back(glm::ivec2(u, v));
				indices.push_back(glm::ivec2(u + 1, v + 1));
				indices.push_back(glm::ivec2(u, v + 1));
			}
		}

		for (const glm::ivec2& index : indices)
		{
			const ControlPoint& control_point = this->m_control_points.at(index.x).at(index.y);

			result.push_back(control_point.vertex.x);
			result.push_back(control_point.vertex.y);
			result.push_back(control_point.vertex.z);

			result.push_back(1.0);
			result.push_back(0.0);
			result.push_back(0.0);

			result.push_back(control_point.uv.x);
			result.push_back(control_point.uv.y);

			result.push_back(1.0);
			result.push_back(0.0);
			result.push_back(0.0);

			result.push_back(1.0);
			result.push_back(0.0);
			result.push_back(0.0);
		}
	}
	else
	{
		result.reserve(this->GetPrimitivesNumValues());

		for (const std::vector<ControlPoint>& control_point_line : this->m_control_points)
		{
			for (const ControlPoint& control_point : control_point_line)
			{
				result.push_back(control_point.vertex.x);
				result.push_back(control_point.vertex.y);
				result.push_back(control_point.vertex.z);

				result.push_back(1.0);
				result.push_back(0.0);
				result.push_back(0.0);

				result.push_back(control_point.uv.x);
				result.push_back(control_point.uv.y);

				result.push_back(1.0);
				result.push_back(0.0);
				result.push_back(0.0);

				result.push_back(1.0);
				result.push_back(0.0);
				result.push_back(0.0);
			}
		}
	}

	return result;
}

std::size_t Patch::GetPrimitivesNumValues() const
{
	if (this->m_interp_mode == Interpolation::None)
	{
		return 6 * (this->m_control_points.size() - 1) * (this->m_control_points.at(0).size() - 1) * GAMEENGINE_VALUES_PER_VERTEX;
	}
	else
	{
		return this->m_control_points.size() * this->m_control_points.at(0).size() * GAMEENGINE_VALUES_PER_VERTEX;
	}
}

Geometry::PrimitiveType Patch::GetPrimitiveType() const
{
	if (this->m_interp_mode == Interpolation::None)
	{
		return Geometry::PrimitiveType::Triangles;
	}
	else
	{
		return Geometry::PrimitiveType::Patch;
	}
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

void Patch::SetInterpolation(Interpolation mode)
{
	if (mode != this->m_interp_mode)
	{
		if (((mode == Interpolation::None) && (this->m_interp_mode != Interpolation::None))
			|| ((mode != Interpolation::None) && (this->m_interp_mode == Interpolation::None)))
		{
			this->InvalidatePrimitivesCache();
		}

		this->m_interp_mode = mode;
	}
}

Patch::Interpolation Patch::GetInterpolation() const
{
	return this->m_interp_mode;
}

Patch::ControlPoint::ControlPoint(glm::dvec3 vertex, glm::dvec2 uv) : vertex(vertex), uv(uv)
{
}
