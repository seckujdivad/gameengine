#pragma once

#include <vector>

const int GAMEENGINE_VALUES_PER_VERTEX = 14;

class Geometry
{
private:
	std::vector<double> m_triangles_cache;
	bool m_triangles_cache_is_valid = false;

protected:
	void InvalidateTriangleCache();
	virtual std::vector<double> GetTrianglesWithoutCache() const = 0;

public:
	virtual ~Geometry();

	std::vector<double> GetTriangles();
	virtual std::size_t GetTrianglesNumValues() const = 0;
};