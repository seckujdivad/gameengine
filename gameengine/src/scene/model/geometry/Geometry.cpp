#include "Geometry.h"

void Geometry::InvalidateTriangleCache()
{
    this->m_triangles_cache_is_valid = false;
}

Geometry::~Geometry()
{
}

std::vector<double> Geometry::GetTriangles()
{
    if (this->m_triangles_cache_is_valid)
    {
        return this->m_triangles_cache;
    }
    else
    {
        this->m_triangles_cache = this->GetTrianglesWithoutCache();
        this->m_triangles_cache_is_valid = true;
        return this->m_triangles_cache;
    }
}
