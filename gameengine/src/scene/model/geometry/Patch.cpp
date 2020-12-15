#include "Patch.h"

std::vector<double> Patch::GetPrimitivesWithoutCache() const
{
	return std::vector<double>(); //TODO: implement patch construction from stored data
}

std::size_t Patch::GetPrimitivesNumValues() const
{
	return std::size_t(); //TODO: implement patch size calculation
}

Geometry::PrimitiveType Patch::GetPrimitiveType() const
{
	return Geometry::PrimitiveType::Patch;
}
