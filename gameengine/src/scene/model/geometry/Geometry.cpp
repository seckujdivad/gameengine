#include "Geometry.h"

void Geometry::InvalidatePrimitivesCache()
{
    this->m_primitives_cache_is_valid = false;
}

Geometry::~Geometry()
{
}

std::vector<double> Geometry::GetPrimitives()
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
