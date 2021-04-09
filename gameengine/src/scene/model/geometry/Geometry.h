#pragma once

#include <vector>
#include <array>
#include <string>

#include <glm/glm.hpp>

const int GAMEENGINE_VALUES_PER_VERTEX = 3 + 3 + 2;

class Geometry
{
public:
	enum class PrimitiveType
	{
		Triangles,
		Quads,
		Patches
	};

	enum class Interpolation
	{
		Linear,
		Bezier
	};

	struct RenderInfo
	{
		bool tesselation_enabled = false;
		Interpolation interpolation_mode = Interpolation::Linear;

		PrimitiveType primitive_type = PrimitiveType::Quads;
		glm::ivec2 primitive_dimensions = glm::ivec2(2, 2);
		std::size_t primitive_size = 4;

		bool operator==(const RenderInfo& second) const;
		bool operator!=(const RenderInfo& second) const;
		bool operator<(const RenderInfo& second) const;
		bool operator<=(const RenderInfo& second) const;
		bool operator>(const RenderInfo& second) const;
		bool operator>=(const RenderInfo& second) const;

		struct Hash
		{
			std::size_t operator()(const RenderInfo& target) const;
		};
	};

private:
	std::vector<double> m_primitives_cache;
	bool m_primitives_cache_is_valid = false;

protected:
	void InvalidatePrimitivesCache();

	virtual std::vector<double> GetPrimitivesWithoutCache() const = 0;

public:
	virtual ~Geometry();

	const std::vector<double>& GetPrimitives();
	virtual std::size_t GetPrimitivesNumVertices() const = 0;
	
	virtual Geometry::PrimitiveType GetPrimitiveType() const = 0;
	virtual std::size_t GetPrimitiveSize() const;
	virtual glm::ivec2 GetPrimitiveDimensions() const = 0;

	virtual Interpolation GetInterpolationMode() const;

	virtual bool GetTesselationEnabled() const;

	RenderInfo GetRenderInfo() const;
};

std::vector<std::array<int, 4>> GetQuadsFromPolygon(std::size_t vertices);
std::vector<std::array<int, 4>> GetQuadsFromPolygon(std::vector<int> indices);

std::size_t GetNumQuadsFromPolygon(std::size_t vertices);

std::string GetPrimitiveTypeName(Geometry::PrimitiveType primitive_type);