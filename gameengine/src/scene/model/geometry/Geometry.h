#pragma once

#include <vector>
#include <array>

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

private:
	std::vector<double> m_primitives_cache;
	bool m_primitives_cache_is_valid = false;

protected:
	void InvalidatePrimitivesCache();

	virtual std::vector<double> GetPrimitivesWithoutCache() const = 0;

public:
	virtual ~Geometry();

	std::vector<double> GetPrimitives();

	virtual std::size_t GetPrimitivesNumVertices() const = 0;
	
	virtual Geometry::PrimitiveType GetPrimitiveType() const = 0;
	virtual std::size_t GetPrimitiveSize() const;
};

std::vector<std::array<int, 4>> GetQuadsFromPolygon(std::size_t vertices);
std::vector<std::array<int, 4>> GetQuadsFromPolygon(std::vector<int> indices);

std::size_t GetNumQuadsFromPolygon(std::size_t vertices);