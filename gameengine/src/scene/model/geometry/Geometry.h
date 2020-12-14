#pragma once

#include <vector>

const int GAMEENGINE_VALUES_PER_VERTEX = 14;

class Geometry
{
public:
	virtual ~Geometry();

	virtual std::vector<double> GetTriangles() const = 0;
	virtual std::size_t GetTrianglesNumValues() const = 0;
};