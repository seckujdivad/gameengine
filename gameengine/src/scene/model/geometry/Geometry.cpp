#include "Geometry.h"

#include <stdexcept>
#include <string>

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

std::size_t Geometry::GetPrimitiveSize() const
{
    switch (this->GetPrimitiveType())
    {
    case PrimitiveType::Triangles:
        return 3;
    case PrimitiveType::Quads:
        return 4;
    }

    throw std::runtime_error("Primitive type " + std::to_string(static_cast<int>(this->GetPrimitiveType())) + " does not have a default size. You must override this method");
}

std::vector<std::array<int, 4>> GetQuadsFromPolygon(std::size_t vertices)
{
    std::vector<int> indices;
    indices.reserve(vertices);
    for (int i = 0; i < static_cast<int>(vertices); i++)
    {
        indices.push_back(i);
    }
    return GetQuadsFromPolygon(indices);
}

std::vector<std::array<int, 4>> GetQuadsFromPolygon(std::vector<int> indices)
{
    std::vector<std::array<int, 4>> result;
    result.reserve(GetNumQuadsFromPolygon(indices.size()));

    if (indices.size() == 0)
    {
        throw std::invalid_argument("Can't represent a non-existent polygon with quads");
    }
    else if (indices.size() == 1)
    {
        throw std::invalid_argument("Can't represent a point with quads");
    }
    if (indices.size() == 2) //line
    {
        std::array<int, 4> quad;
        quad.at(0) = indices.at(0);
        quad.at(1) = indices.at(0);
        quad.at(2) = indices.at(1);
        quad.at(3) = indices.at(1);

        result.push_back(quad);
    }
    else if (indices.size() > 2) //polygon
    {
        int num_non_degenerate_quads = (static_cast<int>(indices.size()) - 2) / 2;
        for (int i = 0; i < num_non_degenerate_quads; i++)
        {
            std::array<int, 4> quad;
            quad.at(0) = indices.at(0);
            quad.at(1) = indices.at((i * 2) + 1);
            quad.at(2) = indices.at((i * 2) + 2);
            quad.at(3) = indices.at((i * 2) + 3);

            result.push_back(quad);
        }

        if (static_cast<int>(indices.size()) % 2 == 1) //there is a single vertex not included, add it as a triangle (degenerate quad)
        {
            std::array<int, 4> quad;
            quad.at(0) = indices.at(0);
            quad.at(1) = indices.at(indices.size() - 2);
            quad.at(2) = indices.at(indices.size() - 1);
            quad.at(3) = indices.at(0);

            result.push_back(quad);
        }
    }
    else
    {
        throw std::runtime_error("Unreachable case");
    }

    return result;
}

std::size_t GetNumQuadsFromPolygon(std::size_t vertices)
{
    if (vertices == 0)
    {
        throw std::invalid_argument("Can't represent a non-existent polygon with quads");
    }
    else if (vertices == 1)
    {
        throw std::invalid_argument("Can't represent a point with quads");
    }
    if (vertices == 2) //line
    {
        return std::size_t(1);
    }
    else if (vertices > 2) //polygon
    {
        std::size_t result = (vertices - 2) / 2;

        if (vertices % 2 == 1)
        {
            result++;
        }

        return result;
    }
    else
    {
        throw std::runtime_error("Unreachable case");
    }

    return std::size_t(-1);
}
