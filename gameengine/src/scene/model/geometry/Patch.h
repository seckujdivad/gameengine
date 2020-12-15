#pragma once

#include <vector>

#include "Geometry.h"

class Patch : public Geometry
{
private:

protected:
	std::vector<double> GetPrimitivesWithoutCache() const override;

public:
	std::size_t GetPrimitivesNumValues() const override;
	Geometry::PrimitiveType GetPrimitiveType() const override;
};