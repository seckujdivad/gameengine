#pragma once

#include <vector>

class Geometry
{
public:
	virtual ~Geometry();

	virtual std::vector<double> GetTriangles() const = 0;
};