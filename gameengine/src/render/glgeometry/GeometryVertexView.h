#pragma once

#include <vector>
#include <set>
#include <memory>

#include "../../scene/model/geometry/Geometry.h"

class GeometryVertexView
{
private:
	std::vector<std::shared_ptr<Geometry>> m_geometries;

public:
	GeometryVertexView(std::vector<std::shared_ptr<Geometry>> geometries);

	std::set<Geometry::RenderInfo> GetRenderInfos() const;

	bool operator==(const GeometryVertexView& right) const;
	bool operator!=(const GeometryVertexView& right) const;

	bool IsEqual(const std::vector<double>& vertices, Geometry::RenderInfo render_info) const;
	std::vector<double> GetVerticesFromRenderInfo(Geometry::RenderInfo render_info) const;
};