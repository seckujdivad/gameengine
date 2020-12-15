#pragma once

#include <vector>

const int GAMEENGINE_VALUES_PER_VERTEX = 14;

class Geometry
{
public:
	enum class PrimitiveType
	{
		Triangles,
		Patch
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
	virtual std::size_t GetPrimitivesNumValues() const = 0;
	virtual Geometry::PrimitiveType GetPrimitiveType() const = 0;
};