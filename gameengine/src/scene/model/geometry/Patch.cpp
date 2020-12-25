#include "Patch.h"

#include <stdexcept>

std::vector<double> Patch::GetPrimitivesWithoutCache() const
{
	std::vector<double> result;

	if (this->m_interp_mode == Interpolation::Linear)
	{
		std::vector<std::array<glm::ivec2, 4>> indices;
		indices.reserve(this->GetPrimitivesNumVertices() / 4);
		result.reserve(this->GetPrimitivesNumVertices());

		for (int u = 0; u < static_cast<int>(this->m_control_points.size()) - 1; u++)
		{
			for (int v = 0; v < static_cast<int>(this->m_control_points.at(0).size()) - 1; v++)
			{
				std::array<glm::ivec2, 4> quad;
				quad.at(0) = glm::ivec2(u, v);
				quad.at(1) = glm::ivec2(u, v + 1);
				quad.at(2) = glm::ivec2(u + 1, v);
				quad.at(3) = glm::ivec2(u + 1, v + 1);
				
				indices.push_back(quad);
			}
		}

		for (const std::array<glm::ivec2, 4>& quad : indices)
		{
			glm::dvec3 normal = glm::dvec3(0.0);
			{
				//calculate tangent and bitangent
				const glm::dvec3 edge1 = this->GetControlPoint(quad.at(1)).vertex - this->GetControlPoint(quad.at(0)).vertex;
				const glm::dvec3 edge2 = this->GetControlPoint(quad.at(2)).vertex - this->GetControlPoint(quad.at(0)).vertex;
				const glm::dvec2 edgeuv1 = this->GetControlPoint(quad.at(1)).uv - this->GetControlPoint(quad.at(0)).uv;
				const glm::dvec2 edgeuv2 = this->GetControlPoint(quad.at(2)).uv - this->GetControlPoint(quad.at(0)).uv;

				const glm::dvec3 tangent = glm::dvec3(
					(edgeuv2.y * edge1.x) - (edgeuv1.y * edge2.x),
					(edgeuv2.y * edge1.y) - (edgeuv1.y * edge2.y),
					(edgeuv2.y * edge1.z) - (edgeuv1.y * edge2.z)
				);

				const glm::dvec3 bitangent = glm::dvec3(
					(edgeuv1.x * edge2.x) - (edgeuv2.x * edge1.x),
					(edgeuv1.x * edge2.y) - (edgeuv2.x * edge1.y),
					(edgeuv1.x * edge2.z) - (edgeuv2.x * edge1.z)
				);

				normal = glm::normalize(glm::cross(tangent, bitangent));
			}

			for (const glm::ivec2& index : quad)
			{
				const ControlPoint& control_point = this->GetControlPoint(index);

				result.push_back(control_point.vertex.x);
				result.push_back(control_point.vertex.y);
				result.push_back(control_point.vertex.z);

				result.push_back(normal.x);
				result.push_back(normal.y);
				result.push_back(normal.z);

				result.push_back(control_point.uv.x);
				result.push_back(control_point.uv.y);
			}
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
	glm::ivec2 dimensions = this->GetPrimitiveDimensions();
	return dimensions.x * dimensions.y;
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
