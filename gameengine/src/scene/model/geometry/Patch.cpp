#include "Patch.h"

#include <stdexcept>

std::vector<double> Patch::GetPrimitivesWithoutCache() const
{
	std::vector<double> result;

	if (this->m_interp_mode == Interpolation::Linear)
	{
		std::vector<glm::ivec2> indices;
		indices.reserve(this->GetPrimitivesNumVertices());
		result.reserve(this->GetPrimitivesNumVertices());

		for (int u = 0; u < static_cast<int>(this->m_control_points.size()) - 1; u++)
		{
			for (int v = 0; v < static_cast<int>(this->m_control_points.at(0).size()) - 1; v++)
			{
				indices.push_back(glm::ivec2(u, v));
				indices.push_back(glm::ivec2(u, v + 1));
				indices.push_back(glm::ivec2(u + 1, v + 1));
				indices.push_back(glm::ivec2(u + 1, v));
			}
		}

		for (const glm::ivec2 index : indices)
		{
			const ControlPoint& control_point = this->m_control_points.at(index.x).at(index.y);

			result.push_back(control_point.vertex.x);
			result.push_back(control_point.vertex.y);
			result.push_back(control_point.vertex.z);

			//TODO: calculate proper surface normals
			result.push_back(1.0);
			result.push_back(0.0);
			result.push_back(0.0);

			result.push_back(control_point.uv.x);
			result.push_back(control_point.uv.y);
		}
	}
	else
	{
		result.reserve(this->GetPrimitivesNumVertices() * std::size_t(GAMEENGINE_VALUES_PER_VERTEX));

		for (const std::vector<ControlPoint>& control_point_line : this->m_control_points)
		{
			for (const ControlPoint& control_point : control_point_line)
			{
				result.push_back(control_point.vertex.x);
				result.push_back(control_point.vertex.y);
				result.push_back(control_point.vertex.z);

				//placeholder normals - they will be calculated from the surface in the tessellation evaluation shader
				result.push_back(1.0);
				result.push_back(0.0);
				result.push_back(0.0);

				result.push_back(control_point.uv.x);
				result.push_back(control_point.uv.y);
			}
		}
	}

	return result;
}

std::size_t Patch::GetPrimitivesNumVertices() const
{
	if (this->m_interp_mode == Interpolation::Linear)
	{
		return GetNumQuadsFromPolygon(4) * (this->m_control_points.size() - 1) * (this->m_control_points.at(0).size() - 1);
	}
	else
	{
		return this->m_control_points.size() * this->m_control_points.at(0).size();
	}
}

Geometry::PrimitiveType Patch::GetPrimitiveType() const
{
	if (this->m_interp_mode == Interpolation::Linear)
	{
		return Geometry::PrimitiveType::Quads;
	}
	else
	{
		return Geometry::PrimitiveType::Patches;
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

Patch::ControlPoint Patch::GetControlPoint(glm::ivec2 index) const
{
	return this->m_control_points.at(index.x).at(index.y);
}

std::size_t Patch::GetPrimitiveSize() const
{
	if (this->m_interp_mode == Interpolation::Linear)
	{
		return 4;
	}
	else
	{
		return this->m_control_points.size() * this->m_control_points.at(0).size();
	}
}

void Patch::SetInterpolationMode(Interpolation mode)
{
	if (mode != this->m_interp_mode)
	{
		if (((mode == Interpolation::Linear) && (this->m_interp_mode != Interpolation::Linear))
			|| ((mode != Interpolation::Linear) && (this->m_interp_mode == Interpolation::Linear)))
		{
			this->InvalidatePrimitivesCache();
		}

		this->m_interp_mode = mode;
	}
}

Patch::Interpolation Patch::GetInterpolationMode() const
{
	return this->m_interp_mode;
}

bool Patch::GetTesselationEnabled() const
{
	return this->m_interp_mode != Interpolation::Linear;
}

glm::ivec2 Patch::GetPrimitiveDimensions() const
{
	if (this->GetPrimitiveType() == PrimitiveType::Quads)
	{
		return glm::ivec2(2, 2);
	}
	else
	{
		return glm::ivec2(
			static_cast<int>(this->m_control_points.size()),
			static_cast<int>(this->m_control_points.at(0).size())
		);
	}
}

Patch::ControlPoint::ControlPoint(glm::dvec3 vertex, glm::dvec2 uv) : vertex(vertex), uv(uv)
{
}
