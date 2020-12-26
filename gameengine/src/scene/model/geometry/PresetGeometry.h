#pragma once

#include "Geometry.h"

#include <memory>
#include <optional>

class PresetGeometry : public Geometry
{
public:
	enum class GeometryType
	{
		Plane
	};

private:
	GeometryType m_type;
	std::shared_ptr<Geometry> m_geometry;

protected:
	std::vector<double> GetPrimitivesWithoutCache() const override;

public:
	PresetGeometry(GeometryType type);

	void SetGeometryType(GeometryType type, bool force_reload = false);
	GeometryType GetGeometryType() const;

	std::size_t GetPrimitivesNumVertices() const override;

	Geometry::PrimitiveType GetPrimitiveType() const override;
	std::size_t GetPrimitiveSize() const override;
	glm::ivec2 GetPrimitiveDimensions() const override;

	Interpolation GetInterpolationMode() const override;

	bool GetTesselationEnabled() const override;
};