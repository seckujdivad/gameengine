#include "GeometryVertexView.h"

#include "../../scene/model/geometry/PresetGeometry.h"

GeometryVertexView::GeometryVertexView(std::vector<std::shared_ptr<Geometry>> geometries) : m_geometries(geometries)
{
}

std::set<Geometry::RenderInfo> GeometryVertexView::GetRenderInfos() const
{
	std::set<Geometry::RenderInfo> result;
	for (const std::shared_ptr<Geometry>& inner_geometry : this->m_geometries)
	{
		if (std::dynamic_pointer_cast<PresetGeometry>(inner_geometry).get() == nullptr)
		{
			Geometry::RenderInfo render_info = inner_geometry->GetRenderInfo();
			result.insert(render_info);
		}
	}
	return result;
}

bool GeometryVertexView::operator==(const GeometryVertexView& right) const
{
	return this->m_geometries == right.m_geometries;
}

bool GeometryVertexView::operator!=(const GeometryVertexView& right) const
{
	return this->m_geometries != right.m_geometries;
}

bool GeometryVertexView::IsEqual(const std::vector<double>& vertices, Geometry::RenderInfo render_info) const
{
	std::size_t num_vertices = vertices.size();
	
	std::vector<std::shared_ptr<Geometry>> filtered_geometries;
	std::size_t num_stored_vertices = 0;

	for (const std::shared_ptr<Geometry>& inner_geometry : this->m_geometries)
	{
		if (std::dynamic_pointer_cast<PresetGeometry>(inner_geometry).get() == nullptr)
		{
			if (render_info == inner_geometry->GetRenderInfo())
			{
				filtered_geometries.push_back(inner_geometry);
				num_stored_vertices += inner_geometry->GetPrimitives().size();
			}
		}
	}
	
	if (num_stored_vertices == num_vertices)
	{
		std::size_t vertices_index = 0;
		for (const std::shared_ptr<Geometry>& inner_geometry : filtered_geometries)
		{
			for (double value : inner_geometry->GetPrimitives())
			{
				if (value != vertices.at(vertices_index))
				{
					return false;
				}

				vertices_index++;
			}
		}

		return true;
	}
	else
	{
		return false;
	}
}

std::vector<double> GeometryVertexView::GetVerticesFromRenderInfo(Geometry::RenderInfo render_info) const
{
	std::vector<double> result;
	for (const std::shared_ptr<Geometry>& inner_geometry : this->m_geometries)
	{
		if (std::dynamic_pointer_cast<PresetGeometry>(inner_geometry).get() == nullptr)
		{
			if (render_info == inner_geometry->GetRenderInfo())
			{
				std::vector<double> data_highp = inner_geometry->GetPrimitives();

				result.reserve(result.size() + data_highp.size());
				for (double value : data_highp)
				{
					result.push_back(value);
				}
			}
		}
	}

	return result;
}
