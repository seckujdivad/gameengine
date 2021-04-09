#include "Geometry.h"

#include <stdexcept>
#include <string>

void Geometry::InvalidatePrimitivesCache()
{
	this->m_primitives_cache_is_valid = false;
}

Geometry::~Geometry()
{
}

const std::vector<double>& Geometry::GetPrimitives()
{
	if (this->m_primitives_cache_is_valid)
	{
		return this->m_primitives_cache;
	}
	else
	{
		this->m_primitives_cache = this->GetPrimitivesWithoutCache();
		this->m_primitives_cache_is_valid = true;
		return this->m_primitives_cache;
	}
}

std::size_t Geometry::GetPrimitiveSize() const
{
	switch (this->GetPrimitiveType())
	{
	case PrimitiveType::Triangles:
		return 3;
	case PrimitiveType::Quads:
		return 4;
	}

	throw std::runtime_error("Primitive type " + std::to_string(static_cast<int>(this->GetPrimitiveType())) + " does not have a default size. You must override this method");
}

Geometry::Interpolation Geometry::GetInterpolationMode() const
{
	return Interpolation::Linear;
}

bool Geometry::GetTesselationEnabled() const
{
	return false;
}

Geometry::RenderInfo Geometry::GetRenderInfo() const
{
	RenderInfo result;
	result.tesselation_enabled = this->GetTesselationEnabled();
	result.interpolation_mode = this->GetInterpolationMode();
	result.primitive_type = this->GetPrimitiveType();
	result.primitive_dimensions = this->GetPrimitiveDimensions();
	result.primitive_size = this->GetPrimitiveSize();

	return result;
}

std::vector<std::array<int, 4>> GetQuadsFromPolygon(std::size_t vertices)
{
	std::vector<int> indices;
	indices.reserve(vertices);
	for (int i = 0; i < static_cast<int>(vertices); i++)
	{
		indices.push_back(i);
	}
	return GetQuadsFromPolygon(indices);
}

std::vector<std::array<int, 4>> GetQuadsFromPolygon(std::vector<int> indices)
{
	std::vector<std::array<int, 4>> result;
	result.reserve(GetNumQuadsFromPolygon(indices.size()));

	if (indices.size() == 0)
	{
		throw std::invalid_argument("Can't represent a non-existent polygon with quads");
	}
	else if (indices.size() == 1)
	{
		throw std::invalid_argument("Can't represent a point with quads");
	}
	if (indices.size() == 2) //line
	{
		std::array<int, 4> quad = {
			indices.at(0),
			indices.at(0),
			indices.at(1),
			indices.at(1)
		};

		result.push_back(quad);
	}
	else if (indices.size() > 2) //polygon
	{
		int num_non_degenerate_quads = (static_cast<int>(indices.size()) - 2) / 2;
		for (int i = 0; i < num_non_degenerate_quads; i++)
		{
			std::array<int, 4> quad = {
				indices.at(0),
				indices.at((i * 2) + 1),
				indices.at((i * 2) + 2),
				indices.at((i * 2) + 3)
			};

			result.push_back(quad);
		}

		if (static_cast<int>(indices.size()) % 2 == 1) //there is a single vertex not included, add it as a triangle (degenerate quad)
		{
			std::array<int, 4> quad = {
				indices.at(0),
				indices.at(indices.size() - 2),
				indices.at(indices.size() - 1),
				indices.at(0)
			};

			result.push_back(quad);
		}
	}
	else
	{
		throw std::runtime_error("Unreachable case");
	}

	//quads are actually drawn as 2x2 linearly interpolated patches, change winding appropriately
	//for a quad:
	//01
	//23
	//the perimeter is 0132 but the patch is 0123, so the 3rd and 4th vertices should be swapped
	for (std::array<int, 4>& quad : result)
	{
		std::swap(quad.at(2), quad.at(3));
	}

	return result;
}

std::size_t GetNumQuadsFromPolygon(std::size_t vertices)
{
	if (vertices == 0)
	{
		throw std::invalid_argument("Can't represent a non-existent polygon with quads");
	}
	else if (vertices == 1)
	{
		throw std::invalid_argument("Can't represent a point with quads");
	}
	if (vertices == 2) //line
	{
		return std::size_t(1);
	}
	else if (vertices > 2) //polygon
	{
		std::size_t result = (vertices - 2) / 2;

		if (vertices % 2 == 1)
		{
			result++;
		}

		return result;
	}
	else
	{
		throw std::runtime_error("Unreachable case");
	}

	return std::size_t(-1);
}

std::string GetPrimitiveTypeName(Geometry::PrimitiveType primitive_type)
{
	switch (primitive_type)
	{
	case Geometry::PrimitiveType::Patches: return "Patches";
	case Geometry::PrimitiveType::Quads: return "Quads";
	case Geometry::PrimitiveType::Triangles: return "Triangles";
	default: throw std::invalid_argument("Unknown primitive type: " + std::to_string(static_cast<int>(primitive_type)));
	}
}

bool Geometry::RenderInfo::operator==(const RenderInfo& second) const
{
	if (this->tesselation_enabled != second.tesselation_enabled)
	{
		return false;
	}

	if (this->interpolation_mode != second.interpolation_mode)
	{
		return false;
	}

	if (this->primitive_type != second.primitive_type)
	{
		return false;
	}

	if (this->primitive_dimensions != second.primitive_dimensions)
	{
		return false;
	}

	if (this->primitive_size != second.primitive_size)
	{
		return false;
	}

	return true;
}

bool Geometry::RenderInfo::operator!=(const RenderInfo& second) const
{
	return !(*this == second);
}

bool Geometry::RenderInfo::operator<(const RenderInfo& second) const
{
	Hash hasher = Hash();
	return hasher(*this) < hasher(second);
}

bool Geometry::RenderInfo::operator<=(const RenderInfo& second) const
{
	Hash hasher = Hash();
	return hasher(*this) <= hasher(second);
}

bool Geometry::RenderInfo::operator>(const RenderInfo& second) const
{
	Hash hasher = Hash();
	return hasher(*this) > hasher(second);
}

bool Geometry::RenderInfo::operator>=(const RenderInfo& second) const
{
	Hash hasher = Hash();
	return hasher(*this) >= hasher(second);
}

std::size_t Geometry::RenderInfo::Hash::operator()(const RenderInfo& target) const
{
	/*
	* Hash layout:
	* Bits		Value
	* 0			tesselation_enabled
	* 1			interpolation_mode
	* 2, 3		primitive_type
	* [4, 7]	primitive_dimensions.x
	* [8, 13]	primitive_dimensions.y
	*/

	int interpolation_mode = static_cast<int>(target.interpolation_mode);
	int primitive_type = static_cast<int>(target.primitive_type);

	//check that the enums haven't changed size (so the bit layout is still valid)
	if (interpolation_mode > 1)
	{
		throw std::runtime_error("Unknown interpolation mode " + std::to_string(interpolation_mode) + ": Geometry::RenderInfo::Hash::operator() needs to be updated");
	}

	if (primitive_type > 2)
	{
		throw std::runtime_error("Unknown interpolation mode " + std::to_string(interpolation_mode) + ": Geometry::RenderInfo::Hash::operator() needs to be updated");
	}

	if (target.primitive_dimensions.x > 15)
	{
		throw std::invalid_argument("Cannot hash when primitive_dimensions.x > 15"); //if this happens, increase the bits allocated
	}

	if (target.primitive_dimensions.y > 15)
	{
		throw std::invalid_argument("Cannot hash when primitive_dimensions.y > 15"); //if this happens, increase the bits allocated
	}

	//calculate hash
	std::size_t result = std::size_t(target.tesselation_enabled);
	result += std::size_t(interpolation_mode) << 1;
	result += std::size_t(primitive_type) << 2;
	result += std::size_t(target.primitive_dimensions.x) << 4;
	result += std::size_t(target.primitive_dimensions.y) << 8;

	return result;
}
